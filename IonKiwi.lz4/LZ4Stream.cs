/*
   lz4.managed - C# translation of lz4
   Copyright (C) 2019-present, Ewout van der Linden.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   lz4.managed
	  - source repository : https://github.com/IonKiwi/lz4.managed
*/

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;

namespace IonKiwi.lz4 {
	public sealed class LZ4Stream : Stream {

		private Stream _innerStream;

		private byte[] _headerBuffer = new byte[23];
		private int _headerBufferSize = 0;
		private int _currentMode = 0;

		private int _targetBufferSize = 0;
		private bool _isCompressed = false;

		private CompressionMode _compressionMode;
		private LZ4FrameBlockSize _blockSize = LZ4FrameBlockSize.Max64KB;
		private LZ4FrameBlockMode _blockMode = LZ4FrameBlockMode.Linked;
		private LZ4FrameChecksumMode _checksumMode = LZ4FrameChecksumMode.None;
		private LZ4StreamMode _streamMode;
		private long? _maxFrameSize = null;
		private bool _highCompression;
		private bool _leaveInnerStreamOpen;
		private bool _hasWrittenStartFrame = false;
		private bool _hasWrittenInitialStartFrame = false;
		private bool _hasFrameInfo = false;
		private ulong _contentSize = 0;
		private long _frameCount = 0;
		private bool _interactiveRead = false;

		private byte[] _inputBufferRef = null;
		private byte[] _outputBufferRef = null;
		private GCHandle _inputBufferHandle;
		private GCHandle _outputBufferHandle;

		private int _outputBufferSize = 0;
		private int _outputBufferOffset = 0;
		private int _outputBufferBlockSize = 0;
		private int _inputBufferSize = 0;
		private int _inputBufferOffset = 0;
		private int _ringbufferOffset = 0;
		private long _blockCount = 0;

		private IDisposable _lz4StreamDispose = null;
		private IDisposable _lz4HCStreamDispose = null;
		private IDisposable _lz4DecodeStreamDispose = null;
		private IDisposable _contentHashStateDispose = null;

		private UnsafeData _unsafeData = new UnsafeData();

		private sealed unsafe class UnsafeData {
			public byte* _inputBufferPtr;
			public byte* _outputBufferPtr;
			public LZ4_stream_u* _lz4Stream = null;
			public LZ4_streamHC_u* _lz4HCStream = null;
			public LZ4_streamDecode_u* _lz4DecodeStream = null;
			public XXH32_state_s* _contentHashState = null;
		}

		public event EventHandler<LZ4UserDataFrameEventArgs> UserDataFrameRead;

		private void OnUserDataFrameRead(LZ4UserDataFrameEventArgs e) {
			EventHandler<LZ4UserDataFrameEventArgs> handler = UserDataFrameRead;
			if (handler != null) {
				handler(this, e);
			}
		}

		private LZ4Stream() {

		}

		public static LZ4Stream CreateCompressor(Stream innerStream, LZ4StreamMode streamMode, LZ4FrameBlockMode blockMode = LZ4FrameBlockMode.Linked, LZ4FrameBlockSize blockSize = LZ4FrameBlockSize.Max1MB, LZ4FrameChecksumMode checksumMode = LZ4FrameChecksumMode.Content, long? maxFrameSize = null, bool highCompression = false, bool leaveInnerStreamOpen = false) {
			if (innerStream == null) { throw new ArgumentNullException(nameof(innerStream)); }
			if (maxFrameSize.HasValue && maxFrameSize.Value <= 0) { throw new ArgumentOutOfRangeException(nameof(maxFrameSize)); }

			LZ4Stream result = new LZ4Stream();
			result._streamMode = streamMode;
			result._innerStream = innerStream;
			result._compressionMode = CompressionMode.Compress;
			result._checksumMode = checksumMode;
			result._blockMode = blockMode;
			result._blockSize = blockSize;
			result._maxFrameSize = maxFrameSize;
			result._leaveInnerStreamOpen = leaveInnerStreamOpen;
			result._highCompression = highCompression;
			result.Init();
			return result;
		}

		public static LZ4Stream CreateDecompressor(Stream innerStream, LZ4StreamMode streamMode, bool leaveInnerStreamOpen = false) {
			if (innerStream == null) { throw new ArgumentNullException(nameof(innerStream)); }

			LZ4Stream result = new LZ4Stream();
			result._streamMode = streamMode;
			result._innerStream = innerStream;
			result._compressionMode = CompressionMode.Decompress;
			result._leaveInnerStreamOpen = leaveInnerStreamOpen;
			result.Init();
			return result;
		}

		public bool InteractiveRead {
			get => _interactiveRead;
			set => _interactiveRead = value;
		}

		public long FrameCount {
			get => _frameCount;
		}

		protected override unsafe void Dispose(bool disposing) {
			try {
				if (disposing) {
					if (_compressionMode == CompressionMode.Compress && _streamMode == LZ4StreamMode.Write) { WriteEndFrameInternal(); }

					if (!_leaveInnerStreamOpen) {
						_innerStream.Dispose();
					}
				}
				base.Dispose(disposing);
			}
			finally {
				if (_inputBufferHandle.IsAllocated) { _inputBufferHandle.Free(); _inputBufferHandle = default; _unsafeData._inputBufferPtr = null; }
				if (_outputBufferHandle.IsAllocated) { _outputBufferHandle.Free(); _outputBufferHandle = default; _unsafeData._outputBufferPtr = null; }

				if (_headerBuffer != null) { ArrayPool<byte>.Shared.Return(_headerBuffer); _headerBuffer = null; }
				if (_inputBufferRef != null) { ArrayPool<byte>.Shared.Return(_inputBufferRef); _inputBufferRef = null; }
				if (_outputBufferRef != null) { ArrayPool<byte>.Shared.Return(_outputBufferRef); _outputBufferRef = null; }

				if (_lz4StreamDispose != null) { _lz4StreamDispose.Dispose(); }
				if (_lz4HCStreamDispose != null) { _lz4HCStreamDispose.Dispose(); }
				if (_contentHashStateDispose != null) { _contentHashStateDispose.Dispose(); }
				if (_lz4DecodeStreamDispose != null) { _lz4DecodeStreamDispose.Dispose(); }
			}
		}

		private unsafe void Init() {
			_headerBuffer = ArrayPool<byte>.Shared.Rent(23);
			if (_compressionMode == CompressionMode.Compress) {
				if (!_highCompression) {
					_unsafeData._lz4Stream = lz4.LZ4_createStream(out _lz4StreamDispose);
				}
				else {
					_unsafeData._lz4HCStream = lz4.LZ4_createStreamHC(out _lz4HCStreamDispose);
				}

				switch (_blockSize) {
					case LZ4FrameBlockSize.Max64KB:
						_inputBufferSize = 64 * (1 << 10);
						_outputBufferSize = 64 * (1 << 10);
						_inputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _inputBufferSize);
						_outputBufferRef = ArrayPool<byte>.Shared.Rent(_outputBufferSize);
						_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
						_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
						_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
						_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
						break;
					case LZ4FrameBlockSize.Max256KB:
						_inputBufferSize = 256 * (1 << 10);
						_outputBufferSize = 256 * (1 << 10);
						_inputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _inputBufferSize);
						_outputBufferRef = ArrayPool<byte>.Shared.Rent(_outputBufferSize);
						_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
						_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
						_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
						_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
						break;
					case LZ4FrameBlockSize.Max1MB:
						_inputBufferSize = 1 * (1 << 20);
						_outputBufferSize = 1 * (1 << 20);
						_inputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _inputBufferSize);
						_outputBufferRef = ArrayPool<byte>.Shared.Rent(_outputBufferSize);
						_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
						_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
						_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
						_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
						break;
					case LZ4FrameBlockSize.Max4MB:
						_inputBufferSize = 4 * (1 << 20);
						_outputBufferSize = 4 * (1 << 20);
						_inputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _inputBufferSize);
						_outputBufferRef = ArrayPool<byte>.Shared.Rent(_outputBufferSize);
						_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
						_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
						_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
						_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
						break;
					default:
						throw new NotSupportedException(_blockSize.ToString());
				}

				if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
					_unsafeData._contentHashState = lz4.XXH32_createState(out _contentHashStateDispose);
					lz4.XXH32_reset(_unsafeData._contentHashState, 0);
				}
			}
			else {
				_unsafeData._lz4DecodeStream = lz4.LZ4_createStreamDecode(out _lz4DecodeStreamDispose);
				_unsafeData._contentHashState = lz4.XXH32_createState(out _contentHashStateDispose);
				lz4.XXH32_reset(_unsafeData._contentHashState, 0);
			}
		}

		public override bool CanRead => _streamMode == LZ4StreamMode.Read;

		public override bool CanSeek => false;

		public override bool CanWrite => _streamMode == LZ4StreamMode.Write;

		public override long Length => -1;

		public override long Position {
			get => -1;
			set => throw new NotSupportedException("SetPosition");
		}

		public override long Seek(long offset, SeekOrigin origin) {
			throw new NotSupportedException(nameof(Seek));
		}

		public override void SetLength(long value) {
			throw new NotSupportedException(nameof(SetLength));
		}

		public override void Flush() {
			if (_compressionMode == CompressionMode.Compress && _streamMode == LZ4StreamMode.Write && _inputBufferOffset > 0) {
				FlushCurrentBlock(false);
			}
		}

		public void WriteEndFrame() {
			if (!(_compressionMode == CompressionMode.Compress && _streamMode == LZ4StreamMode.Write)) {
				throw new NotSupportedException("Only supported in compress mode with a write mode stream");
			}
			WriteEndFrameInternal();
		}

		private void WriteHeaderData(byte[] buffer, int offset, int count) {
			if (_streamMode == LZ4StreamMode.Read) {
				Buffer.BlockCopy(buffer, offset, _headerBuffer, _headerBufferSize, count);
				_headerBufferSize += count;
			}
			else {
				_innerStream.Write(buffer, offset, count);
			}
		}

		private unsafe void WriteEndFrameInternal() {
			if (!_hasWrittenInitialStartFrame) {
				//WriteEmptyFrame();
				return;
			}

			if (!_hasWrittenStartFrame) {
				return;
				//throw new InvalidOperationException("No start frame was written");
			}

			if (_inputBufferOffset > 0) {
				// flush current block first
				FlushCurrentBlock(true);
			}

			byte[] b = new byte[4];

			// write end mark
			b[0] = 0;
			b[1] = 0;
			b[2] = 0;
			b[3] = 0; // 0x80
			WriteHeaderData(b, 0, b.Length);

			if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
				uint xxh = lz4.XXH32_digest(_unsafeData._contentHashState);
				lz4.XXH32_reset(_unsafeData._contentHashState, 0); // reset for next frame

				b[0] = (byte)(xxh & 0xFF);
				b[1] = (byte)((xxh >> 8) & 0xFF);
				b[2] = (byte)((xxh >> 16) & 0xFF);
				b[3] = (byte)((xxh >> 24) & 0xFF);
				WriteHeaderData(b, 0, b.Length);
			}

			// reset the stream
			if (!_highCompression) {
				lz4.LZ4_loadDict(_unsafeData._lz4Stream, null, 0);
			}
			else {
				lz4.LZ4_loadDictHC(_unsafeData._lz4HCStream, null, 0);
			}

			_hasWrittenStartFrame = false;
		}

		private unsafe void WriteStartFrame() {
			_hasWrittenStartFrame = true;
			_hasWrittenInitialStartFrame = true;
			_frameCount++;
			_blockCount = 0;
			//_ringbufferOffset = 0;

			// write magic
			byte[] magic = new byte[4];
			magic[0] = 0x04;
			magic[1] = 0x22;
			magic[2] = 0x4D;
			magic[3] = 0x18;
			WriteHeaderData(magic, 0, magic.Length);

			// frame descriptor
			byte[] descriptor = new byte[2]; // if _storeContentSize +8
			descriptor[0] = 0;
			if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
				descriptor[0] |= 0x4;
			}

			//if (_storeContentSize) {
			//	descriptor[0] |= 0x8;
			//}

			if ((_checksumMode & LZ4FrameChecksumMode.Block) == LZ4FrameChecksumMode.Block) {
				descriptor[0] |= 0x10;
			}

			if (_blockMode == LZ4FrameBlockMode.Independent) {
				descriptor[0] |= 0x20;
			}

			descriptor[0] |= 0x40; // version 01

			descriptor[1] = 0;
			if (_blockSize == LZ4FrameBlockSize.Max64KB) {
				descriptor[1] |= (4 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max256KB) {
				descriptor[1] |= (5 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max1MB) {
				descriptor[1] |= (6 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max4MB) {
				descriptor[1] |= (7 << 4);
			}
			else {
				throw new NotImplementedException(_blockSize.ToString());
			}

			//if (_storeContentSize) {
			//	unsigned long long contentsize = 0; // we cant support this, as the contentsize is not known in advance
			//	descriptor[2] = (byte)(contentsize & 0xFF);
			//	descriptor[3] = (byte)((contentsize >> 8) & 0xFF);
			//	descriptor[4] = (byte)((contentsize >> 16) & 0xFF);
			//	descriptor[5] = (byte)((contentsize >> 24) & 0xFF);
			//	descriptor[6] = (byte)((contentsize >> 32) & 0xFF);
			//	descriptor[7] = (byte)((contentsize >> 40) & 0xFF);
			//	descriptor[8] = (byte)((contentsize >> 48) & 0xFF);
			//	descriptor[9] = (byte)((contentsize >> 56) & 0xFF);
			//}

			uint xxh;
			fixed (byte* descriptorPtr = &descriptor[0]) {
				xxh = lz4.XXH32(descriptorPtr, (uint)descriptor.Length, 0);
			}

			WriteHeaderData(descriptor, 0, descriptor.Length);
			byte[] checksumByte = new byte[1];
			checksumByte[0] = (byte)((xxh >> 8) & 0xFF);
			WriteHeaderData(checksumByte, 0, 1);
		}

		private unsafe void WriteEmptyFrame() {
			if (_compressionMode != CompressionMode.Compress) { throw new NotSupportedException("Only supported in compress mode"); }

			if (_hasWrittenStartFrame || _hasWrittenInitialStartFrame) {
				throw new InvalidOperationException("should not have happend, hasWrittenStartFrame: " + _hasWrittenStartFrame + ", hasWrittenInitialStartFrame: " + _hasWrittenInitialStartFrame);
			}

			// write magic
			byte[] magic = new byte[4];
			magic[0] = 0x04;
			magic[1] = 0x22;
			magic[2] = 0x4D;
			magic[3] = 0x18;
			WriteHeaderData(magic, 0, magic.Length);

			// frame descriptor
			byte[] descriptor = new byte[2];
			descriptor[0] = 0;
			if (_blockMode == LZ4FrameBlockMode.Independent) { descriptor[0] |= 0x20; }
			descriptor[0] |= 0x40; // version 01

			descriptor[1] = 0;
			if (_blockSize == LZ4FrameBlockSize.Max64KB) {
				descriptor[1] |= (4 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max256KB) {
				descriptor[1] |= (5 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max1MB) {
				descriptor[1] |= (6 << 4);
			}
			else if (_blockSize == LZ4FrameBlockSize.Max4MB) {
				descriptor[1] |= (7 << 4);
			}
			else {
				throw new NotImplementedException(_blockSize.ToString());
			}

			uint xxh;
			fixed (byte* descriptorPtr = &descriptor[0]) {
				xxh = lz4.XXH32(descriptorPtr, (uint)descriptor.Length, 0);
			}

			WriteHeaderData(descriptor, 0, descriptor.Length);
			byte[] checksumByte = new byte[1];
			checksumByte[0] = (byte)((xxh >> 8) & 0xFF);
			WriteHeaderData(checksumByte, 0, 1);

			byte[] endMarker = new byte[4];
			WriteHeaderData(endMarker, 0, endMarker.Length);

			_hasWrittenInitialStartFrame = true;
			_frameCount++;
		}

		public void WriteUserDataFrame(int id, byte[] buffer, int offset, int count) {
			if (!(_compressionMode == CompressionMode.Compress && _streamMode == LZ4StreamMode.Write)) {
				throw new NotSupportedException("Only supported in compress mode with a write mode stream");
			}
			WriteUserDataFrameInternal(id, buffer, offset, count);
		}

		private void WriteUserDataFrameInternal(int id, byte[] buffer, int offset, int count) {
			if (id < 0 || id > 15) { throw new ArgumentOutOfRangeException("id"); }
			else if (buffer == null) { throw new ArgumentNullException("buffer"); }
			else if (count < 0) { throw new ArgumentOutOfRangeException("count"); }
			else if (offset < 0) { throw new ArgumentOutOfRangeException("offset"); }
			else if (offset + count > buffer.Length) { throw new ArgumentOutOfRangeException("offset + count"); }

			if (!_hasWrittenInitialStartFrame) {
				// write empty frame according to spec recommendation
				WriteEmptyFrame();
			}

			if (_hasWrittenStartFrame) {
				WriteEndFrameInternal();
			}

			// write magic
			byte[] magic = new byte[4];
			magic[0] = (byte)(0x50 + id);
			magic[1] = 0x2A;
			magic[2] = 0x4D;
			magic[3] = 0x18;
			_innerStream.Write(magic, 0, magic.Length);

			// write size
			byte[] b = new byte[4];
			b[0] = (byte)((uint)count & 0xFF);
			b[1] = (byte)(((uint)count >> 8) & 0xFF);
			b[2] = (byte)(((uint)count >> 16) & 0xFF);
			b[3] = (byte)(((uint)count >> 24) & 0xFF);
			_innerStream.Write(b, 0, b.Length);

			// write data
			_innerStream.Write(buffer, offset, count);

			_frameCount++;
		}

		private unsafe void FlushCurrentBlock(bool suppressEndFrame) {

			//char* inputBufferPtr = &_inputBufferPtr[_ringbufferOffset];
			//char* outputBufferPtr = &_outputBufferPtr[0];

			if (!_hasWrittenStartFrame) {
				WriteStartFrame();
			}

			if (_blockMode == LZ4FrameBlockMode.Independent || _blockCount == 0) {
				// reset the stream { create independently compressed blocks }
				if (!_highCompression) {
					lz4.LZ4_loadDict(_unsafeData._lz4Stream, null, 0);
				}
				else {
					lz4.LZ4_loadDictHC(_unsafeData._lz4HCStream, null, 0);
				}
			}

			int targetSize;
			bool isCompressed;

			if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
				XXH_errorcode status = lz4.XXH32_update(_unsafeData._contentHashState, &_unsafeData._inputBufferPtr[_ringbufferOffset], (uint)_inputBufferOffset);
				if (status != XXH_errorcode.XXH_OK) {
					throw new Exception("Failed to update content checksum");
				}
			}

			int outputBytes;
			if (!_highCompression) {
				outputBytes = lz4.LZ4_compress_fast_continue(_unsafeData._lz4Stream, &_unsafeData._inputBufferPtr[_ringbufferOffset], &_unsafeData._outputBufferPtr[0], _inputBufferOffset, _outputBufferSize, 1);
			}
			else {
				outputBytes = lz4.LZ4_compress_HC_continue(_unsafeData._lz4HCStream, &_unsafeData._inputBufferPtr[_ringbufferOffset], &_unsafeData._outputBufferPtr[0], _inputBufferOffset, _outputBufferSize);
			}

			if (outputBytes == 0) {
				// compression failed or output is too large

				// reset the stream
				//if (!_highCompression) {
				//	LZ4_loadDict(_lz4Stream, null, 0);
				//}
				//else {
				//	LZ4_loadDictHC(_lz4HCStream, null, 0);
				//}

				Unsafe.CopyBlock(_unsafeData._outputBufferPtr, &_unsafeData._inputBufferPtr[_ringbufferOffset], (uint)_inputBufferOffset);
				targetSize = _inputBufferOffset;
				isCompressed = false;
			}
			else if (outputBytes >= _inputBufferOffset) {
				// compressed size is bigger than input size

				// reset the stream
				//if (!_highCompression) {
				//	LZ4_loadDict(_lz4Stream, null, 0);
				//}
				//else {
				//	LZ4_loadDictHC(_lz4HCStream, null, 0);
				//}

				Unsafe.CopyBlock(_unsafeData._outputBufferPtr, &_unsafeData._inputBufferPtr[_ringbufferOffset], (uint)_inputBufferOffset);
				targetSize = _inputBufferOffset;
				isCompressed = false;
			}
			else if (outputBytes < 0) {
				throw new Exception("Compress failed");
			}
			else {
				targetSize = outputBytes;
				isCompressed = true;
			}

			byte[] b = new byte[4];
			b[0] = (byte)((uint)targetSize & 0xFF);
			b[1] = (byte)(((uint)targetSize >> 8) & 0xFF);
			b[2] = (byte)(((uint)targetSize >> 16) & 0xFF);
			b[3] = (byte)(((uint)targetSize >> 24) & 0xFF);

			if (!isCompressed) {
				b[3] |= 0x80;
			}

			_innerStream.Write(b, 0, b.Length);
			_innerStream.Write(_outputBufferRef, 0, targetSize);

			if ((_checksumMode & LZ4FrameChecksumMode.Block) == LZ4FrameChecksumMode.Block) {
				uint xxh = lz4.XXH32(&_unsafeData._outputBufferPtr[0], (uint)targetSize, 0);

				b[0] = (byte)(xxh & 0xFF);
				b[1] = (byte)((xxh >> 8) & 0xFF);
				b[2] = (byte)((xxh >> 16) & 0xFF);
				b[3] = (byte)((xxh >> 24) & 0xFF);
				_innerStream.Write(b, 0, b.Length);
			}

			_inputBufferOffset = 0; // reset before calling WriteEndFrame() !!
			_blockCount++;

			if (!suppressEndFrame && _maxFrameSize.HasValue && _blockCount >= _maxFrameSize.Value) {
				WriteEndFrameInternal();
			}

			// update ringbuffer offset
			_ringbufferOffset += _inputBufferSize;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _inputBufferSize) _ringbufferOffset = 0;
		}

		private unsafe bool GetFrameInfo() {

			if (_hasFrameInfo) {
				throw new Exception("should not have happend, _hasFrameInfo: " + _hasFrameInfo);
			}

			byte[] magic = new byte[4];
			int bytesRead = _innerStream.Read(magic, 0, magic.Length);
			if (bytesRead == 0) {
				return false;
			}
			else if (bytesRead != magic.Length) { throw new EndOfStreamException("Unexpected end of stream"); }

			_blockCount = 0;
			if (magic[0] == 0x04 && magic[1] == 0x22 && magic[2] == 0x4D && magic[3] == 0x18) {
				// lz4 frame
				_frameCount++;

				// reset state
				lz4.XXH32_reset(_unsafeData._contentHashState, 0);
				_blockSize = LZ4FrameBlockSize.Max64KB;
				_blockMode = LZ4FrameBlockMode.Linked;
				_checksumMode = LZ4FrameChecksumMode.None;
				_contentSize = 0;
				_outputBufferOffset = 0;
				_outputBufferBlockSize = 0;
				//_ringbufferOffset = 0;

				// read frame descriptor
				byte[] descriptor = new byte[2];
				bytesRead = _innerStream.Read(descriptor, 0, descriptor.Length);
				if (bytesRead != descriptor.Length) { throw new EndOfStreamException("Unexpected end of stream"); }

				// verify version
				if (!((descriptor[0] & 0x40) == 0x40 || (descriptor[0] & 0x60) == 0x60) || (descriptor[0] & 0x80) != 0x00) {
					throw new Exception("Unexpected frame version");
				}
				else if ((descriptor[0] & 0x01) != 0x00) {
					throw new Exception("Predefined dictionaries are not supported");
				}
				else if ((descriptor[0] & 0x02) != 0x00) {
					// reserved value
					throw new Exception("Header contains unexpected value");
				}

				if ((descriptor[0] & 0x04) == 0x04) {
					_checksumMode = _checksumMode | LZ4FrameChecksumMode.Content;
				}
				bool hasContentSize = false;
				if ((descriptor[0] & 0x08) == 0x08) {
					hasContentSize = true;
				}
				if ((descriptor[0] & 0x10) == 0x10) {
					_checksumMode = _checksumMode | LZ4FrameChecksumMode.Block;
				}
				if ((descriptor[0] & 0x20) == 0x20) {
					_blockMode = LZ4FrameBlockMode.Independent;
				}

				if ((descriptor[1] & 0x0F) != 0x00) {
					// reserved value
					throw new Exception("Header contains unexpected value");
				}
				else if ((descriptor[1] & 0x80) != 0x00) {
					// reserved value
					throw new Exception("Header contains unexpected value");
				}

				int blockSizeId = (descriptor[1] & 0x70) >> 4;
				if (blockSizeId == 4) {
					_blockSize = LZ4FrameBlockSize.Max64KB;
					_inputBufferSize = 64 * (1 << 10);
					_outputBufferSize = 64 * (1 << 10);
				}
				else if (blockSizeId == 5) {
					_blockSize = LZ4FrameBlockSize.Max256KB;
					_inputBufferSize = 256 * (1 << 10);
					_outputBufferSize = 256 * (1 << 10);
				}
				else if (blockSizeId == 6) {
					_blockSize = LZ4FrameBlockSize.Max1MB;
					_inputBufferSize = 1 * (1 << 20);
					_outputBufferSize = 1 * (1 << 20);
				}
				else if (blockSizeId == 7) {
					_blockSize = LZ4FrameBlockSize.Max4MB;
					_inputBufferSize = 4 * (1 << 20);
					_outputBufferSize = 4 * (1 << 20);
				}
				else {
					throw new Exception("Unsupported block size: " + blockSizeId);
				}

				if (hasContentSize) {
					byte tmp1 = descriptor[0], tmp2 = descriptor[1];
					descriptor = new byte[2 + 8];
					descriptor[0] = tmp1;
					descriptor[1] = tmp2;
					bytesRead = _innerStream.Read(descriptor, 2, 8);
					if (bytesRead != 8) { throw new EndOfStreamException("Unexpected end of stream"); }

					_contentSize = 0;
					for (int i = 9; i >= 2; i--) {
						_contentSize |= ((ulong)descriptor[i] << (i * 8));
					}
				}

				// read checksum
				byte[] checksum = new byte[1];
				bytesRead = _innerStream.Read(checksum, 0, checksum.Length);
				if (bytesRead != checksum.Length) { throw new EndOfStreamException("Unexpected end of stream"); }
				// verify checksum
				uint xxh;
				fixed (byte* descriptorPtr = &descriptor[0]) {
					xxh = lz4.XXH32(descriptorPtr, (uint)descriptor.Length, 0);
				}
				byte checksumByte = (byte)((xxh >> 8) & 0xFF);
				if (checksum[0] != checksumByte) {
					throw new Exception("Frame checksum is invalid");
				}

				// resize buffers
				if (_inputBufferRef == null || _inputBufferRef.Length < _inputBufferSize) {
					if (_inputBufferHandle.IsAllocated) { _inputBufferHandle.Free(); _inputBufferHandle = default; _unsafeData._inputBufferPtr = null; }
					if (_inputBufferRef != null) { ArrayPool<byte>.Shared.Return(_inputBufferRef); _inputBufferRef = null; }
					_inputBufferRef = ArrayPool<byte>.Shared.Rent(_inputBufferSize);
					_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
					_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
				}
				if (_outputBufferRef == null || _outputBufferRef.Length < 2 * _outputBufferSize) {
					if (_outputBufferHandle.IsAllocated) { _outputBufferHandle.Free(); _outputBufferHandle = default; _unsafeData._outputBufferPtr = null; }
					if (_outputBufferRef != null) { ArrayPool<byte>.Shared.Return(_outputBufferRef); _outputBufferRef = null; }
					_outputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _outputBufferSize);
					_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
					_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
				}

				_hasFrameInfo = true;
				return true;
			}
			else if (magic[0] >= 0x50 && magic[0] <= 0x5f && magic[1] == 0x2A && magic[2] == 0x4D && magic[3] == 0x18) {
				// skippable frame
				_frameCount++;

				// read frame size
				byte[] b = new byte[4];
				bytesRead = _innerStream.Read(b, 0, b.Length);
				if (bytesRead != b.Length) {
					throw new EndOfStreamException("Unexpected end of stream");
				}

				uint frameSize = 0;
				for (int i = b.Length - 1; i >= 0; i--) {
					frameSize |= ((uint)b[i] << (i * 8));
				}

				byte[] userData = new byte[frameSize];
				bytesRead = _innerStream.Read(userData, 0, userData.Length);
				if (bytesRead != userData.Length) { throw new EndOfStreamException("Unexpected end of stream"); }

				int id = (magic[0] & 0xF);

				var handler = UserDataFrameRead;
				if (handler != null) {
					LZ4UserDataFrameEventArgs e = new LZ4UserDataFrameEventArgs(id, userData);
					handler(this, e);
				}

				// read next frame header
				return GetFrameInfo();
			}
			else {
				throw new Exception("lz4 stream is corrupt");
			}
		}

		private unsafe bool AcquireNextBlock() {
			if (!_hasFrameInfo) {
				if (!GetFrameInfo()) {
					return false;
				}
			}

			byte[] b = new byte[4];

			// read block size
			int bytesRead = _innerStream.Read(b, 0, b.Length);
			if (bytesRead != b.Length) { throw new EndOfStreamException("Unexpected end of stream"); }

			bool isCompressed = true;
			if ((b[3] & 0x80) == 0x80) {
				isCompressed = false;
				b[3] &= 0x7F;
			}

			uint blockSize = 0;
			for (int i = b.Length - 1; i >= 0; i--) {
				blockSize |= ((uint)b[i] << (i * 8));
			}

			if (blockSize > (uint)_outputBufferSize) {
				throw new Exception("Block size exceeds maximum block size");
			}

			if (blockSize == 0) {
				// end marker

				//_outputBufferSize = 0;
				//_outputBufferOffset = 0;
				//_outputBufferBlockSize = 0;
				//_inputBufferSize = 0;
				//_inputBufferOffset = 0;
				_hasFrameInfo = false;

				if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
					// calculate hash
					uint xxh = lz4.XXH32_digest(_unsafeData._contentHashState);

					// read hash
					b = new byte[4];
					bytesRead = _innerStream.Read(b, 0, b.Length);
					if (bytesRead != b.Length) {
						throw new EndOfStreamException("Unexpected end of stream");
					}

					if (b[0] != (byte)(xxh & 0xFF) ||
						b[1] != (byte)((xxh >> 8) & 0xFF) ||
						b[2] != (byte)((xxh >> 16) & 0xFF) ||
						b[3] != (byte)((xxh >> 24) & 0xFF)) {
						throw new Exception("Content checksum did not match");
					}
				}

				return AcquireNextBlock();
			}

			// read block data
			bytesRead = _innerStream.Read(_inputBufferRef, 0, (int)blockSize);
			if (bytesRead != blockSize) { throw new EndOfStreamException("Unexpected end of stream"); }

			_blockCount++;

			if ((_checksumMode & LZ4FrameChecksumMode.Block) == LZ4FrameChecksumMode.Block) {
				// read block checksum
				b = new byte[4];
				bytesRead = _innerStream.Read(b, 0, b.Length);
				if (bytesRead != b.Length) { throw new EndOfStreamException("Unexpected end of stream"); }

				uint checksum = 0;
				for (int i = b.Length - 1; i >= 0; i--) {
					checksum |= ((uint)b[i] << (i * 8));
				}
				// verify checksum
				uint xxh = lz4.XXH32(&_unsafeData._inputBufferPtr[0], blockSize, 0);
				if (checksum != xxh) {
					throw new Exception("Block checksum did not match");
				}
			}

			int currentRingbufferOffset = _ringbufferOffset;

			// preserve previously compressed block data (for LZ4_decompress_safe_continue dictionary [LZ4FrameBlockMode.Linked])
			// update ringbuffer offset
			_ringbufferOffset += _outputBufferSize;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _outputBufferSize) _ringbufferOffset = 0;

			if (!isCompressed) {
				Unsafe.CopyBlock(&_unsafeData._outputBufferPtr[_ringbufferOffset], _unsafeData._inputBufferPtr, blockSize);
				_outputBufferBlockSize = (int)blockSize;
			}
			else {
				byte* inputPtr = &_unsafeData._inputBufferPtr[0];
				byte* outputPtr = &_unsafeData._outputBufferPtr[_ringbufferOffset];
				byte* dict = &_unsafeData._outputBufferPtr[currentRingbufferOffset];
				int status;
				if (_blockCount > 1) {
					status = lz4.LZ4_setStreamDecode(_unsafeData._lz4DecodeStream, dict, _outputBufferSize);
				}
				else {
					status = lz4.LZ4_setStreamDecode(_unsafeData._lz4DecodeStream, null, 0);
				}
				if (status != 1) {
					throw new Exception("LZ4_setStreamDecode failed");
				}
				int decompressedSize = lz4.LZ4_decompress_safe_continue(_unsafeData._lz4DecodeStream, inputPtr, outputPtr, (int)blockSize, _outputBufferSize);
				if (decompressedSize <= 0) {
					throw new Exception("Decompress failed");
				}
				_outputBufferBlockSize = decompressedSize;
			}

			if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
				void* contentPtr = &_unsafeData._outputBufferPtr[_ringbufferOffset];
				XXH_errorcode status = lz4.XXH32_update(_unsafeData._contentHashState, contentPtr, (uint)_outputBufferBlockSize);
				if (status != XXH_errorcode.XXH_OK) {
					throw new Exception("Failed to update content checksum");
				}
			}

			_outputBufferOffset = 0;

			return true;
		}

		private unsafe void CompressNextBlock() {

			// write at least one start frame
			//if (!_hasWrittenInitialStartFrame) { WriteStartFrame(); }

			int chunk = _inputBufferSize - _inputBufferOffset;
			if (chunk == 0) { throw new Exception("should not have happend, Read(): compress, chunk == 0"); }

			// compress 1 block
			int bytesRead;
			do {
				bytesRead = _innerStream.Read(_inputBufferRef, _ringbufferOffset + _inputBufferOffset, chunk);
				if (bytesRead == 0) {
					_isCompressed = true;
					break;
				}
				else {
					_inputBufferOffset += bytesRead;
					chunk -= bytesRead;
				}
			} while (chunk > 0);

			if (_inputBufferOffset == 0) {
				return;
			}

			_headerBufferSize = 0;
			_outputBufferOffset = 0;

			byte* inputBufferPtr = &_unsafeData._inputBufferPtr[_ringbufferOffset];
			byte* outputBufferPtr = &_unsafeData._outputBufferPtr[0];

			if (!_hasWrittenStartFrame) {
				WriteStartFrame();
			}

			if (_blockMode == LZ4FrameBlockMode.Independent || _blockCount == 0) {
				// reset the stream { create independently compressed blocks }
				if (!_highCompression) {
					lz4.LZ4_loadDict(_unsafeData._lz4Stream, null, 0);
				}
				else {
					lz4.LZ4_loadDictHC(_unsafeData._lz4HCStream, null, 0);
				}
			}

			int targetSize;
			bool isCompressed;

			if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
				XXH_errorcode status = lz4.XXH32_update(_unsafeData._contentHashState, inputBufferPtr, (uint)_inputBufferOffset);
				if (status != XXH_errorcode.XXH_OK) {
					throw new Exception("Failed to update content checksum");
				}
			}

			int outputBytes;
			if (!_highCompression) {
				outputBytes = lz4.LZ4_compress_fast_continue(_unsafeData._lz4Stream, inputBufferPtr, outputBufferPtr, _inputBufferOffset, _outputBufferSize, 1);
			}
			else {
				outputBytes = lz4.LZ4_compress_HC_continue(_unsafeData._lz4HCStream, inputBufferPtr, outputBufferPtr, _inputBufferOffset, _outputBufferSize);
			}
			if (outputBytes == 0) {
				// compression failed or output is too large

				// reset the stream
				//if (!_highCompression) {
				//	LZ4_loadDict(_lz4Stream, null, 0);
				//}
				//else {
				//	LZ4_loadDictHC(_lz4HCStream, null, 0);
				//}

				Unsafe.CopyBlock(_unsafeData._outputBufferPtr, &_unsafeData._inputBufferPtr[_ringbufferOffset], (uint)_inputBufferOffset);
				targetSize = _inputBufferOffset;
				isCompressed = false;
			}
			else if (outputBytes >= _inputBufferOffset) {
				// compressed size is bigger than input size

				// reset the stream
				//if (!_highCompression) {
				//	LZ4_loadDict(_lz4Stream, null, 0);
				//}
				//else {
				//	LZ4_loadDictHC(_lz4HCStream, null, 0);
				//}

				Unsafe.CopyBlock(_unsafeData._outputBufferPtr, &_unsafeData._inputBufferPtr[_ringbufferOffset], (uint)_inputBufferOffset);
				targetSize = _inputBufferOffset;
				isCompressed = false;
			}
			else if (outputBytes < 0) {
				throw new Exception("Compress failed");
			}
			else {
				targetSize = outputBytes;
				isCompressed = true;
			}

			byte[] b = new byte[4];
			_headerBuffer[_headerBufferSize++] = (byte)((uint)targetSize & 0xFF);
			_headerBuffer[_headerBufferSize++] = (byte)(((uint)targetSize >> 8) & 0xFF);
			_headerBuffer[_headerBufferSize++] = (byte)(((uint)targetSize >> 16) & 0xFF);
			_headerBuffer[_headerBufferSize++] = (byte)(((uint)targetSize >> 24) & 0xFF);

			if (!isCompressed) {
				_headerBuffer[_headerBufferSize - 1] |= 0x80;
			}

			_outputBufferBlockSize = targetSize;
			_currentMode = 1;
		}

		private unsafe int CompressData(byte[] buffer, int offset, int count) {
			// _currentMode == 0 . frame start
			// _currentMode == 1 . copy header data
			// _currentMode == 2 . block data
			// _currentMode == 3 . copy header data after block

			int consumed = 0, total = 0;
			do {
				offset += consumed;
				count -= consumed;
				consumed = 0;

				if (_currentMode == 0) {
					if (!_isCompressed) {
						CompressNextBlock();
					}
					else if (_hasWrittenStartFrame) {
						WriteEndFrameInternal();
						_currentMode = 4;
					}
					else {
						return total;
					}
				}
				else if (_currentMode == 1) {
					if (_headerBufferSize == 0) { throw new Exception("should not have happend, Read(): compress, _headerBufferSize == 0"); }
					int chunk = consumed = Math.Min(_headerBufferSize - _outputBufferOffset, count);
					Buffer.BlockCopy(_headerBuffer, _outputBufferOffset, buffer, offset, chunk);
					_outputBufferOffset += chunk;
					if (_outputBufferOffset == _headerBufferSize) {
						_outputBufferOffset = 0;
						_currentMode = 2;
					}
				}
				else if (_currentMode == 2) {
					if (_outputBufferBlockSize == 0) { throw new Exception("should not have happend, Read(): compress, _outputBufferBlockSize == 0"); }
					int chunk = consumed = Math.Min(_outputBufferBlockSize - _outputBufferOffset, count);
					Buffer.BlockCopy(_outputBufferRef, _outputBufferOffset, buffer, offset, chunk);
					_outputBufferOffset += chunk;
					if (_outputBufferOffset == _outputBufferBlockSize) {
						_inputBufferOffset = 0; // reset before calling WriteEndFrame() !!
						_blockCount++;
						_currentMode = 3;
					}
				}
				else if (_currentMode == 3) {
					_currentMode = 0;

					_headerBufferSize = 0;
					_outputBufferOffset = 0;

					if ((_checksumMode & LZ4FrameChecksumMode.Block) == LZ4FrameChecksumMode.Block) {

						uint xxh = lz4.XXH32(&_unsafeData._outputBufferPtr[0], (uint)_outputBufferBlockSize, 0);

						_headerBuffer[_headerBufferSize++] = (byte)(xxh & 0xFF);
						_headerBuffer[_headerBufferSize++] = (byte)((xxh >> 8) & 0xFF);
						_headerBuffer[_headerBufferSize++] = (byte)((xxh >> 16) & 0xFF);
						_headerBuffer[_headerBufferSize++] = (byte)((xxh >> 24) & 0xFF);
						_currentMode = 4;
					}

					if (_maxFrameSize.HasValue && _blockCount >= _maxFrameSize.Value) {
						WriteEndFrameInternal();
						_currentMode = 4;
					}

					// update ringbuffer offset
					_ringbufferOffset += _inputBufferSize;
					// wraparound the ringbuffer offset
					if (_ringbufferOffset > _inputBufferSize) _ringbufferOffset = 0;
				}
				else if (_currentMode == 4) {
					if (_headerBufferSize == 0) { throw new Exception("should not have happend, Read(): compress, _headerBufferSize == 0"); }
					int chunk = consumed = Math.Min(_headerBufferSize - _outputBufferOffset, count);
					Buffer.BlockCopy(_headerBuffer, _outputBufferOffset, buffer, offset, chunk);
					_outputBufferOffset += chunk;
					if (_outputBufferOffset == _headerBufferSize) {
						_outputBufferOffset = 0;
						_headerBufferSize = 0;
						_currentMode = 0;
					}
				}
				else {
					throw new Exception("should not have happend, Read(): compress, _currentmode == " + _currentMode);
				}

				total += consumed;
			} while (!_interactiveRead && consumed < count);

			return total;
		}

		public override int ReadByte() {
			if (_streamMode != LZ4StreamMode.Read) { throw new NotSupportedException("Read"); }

			if (_compressionMode == CompressionMode.Decompress) {
				if (_outputBufferOffset >= _outputBufferBlockSize && !AcquireNextBlock()) {
					return -1; // end of stream
				}
				return _outputBufferRef[_ringbufferOffset + _outputBufferOffset++];
			}
			else {
				byte[] data = new byte[1];
				int x = CompressData(data, 0, 1);
				if (x == 0) {
					return -1; // stream end
				}
				return data[0];
			}
		}

		public override int Read(byte[] buffer, int offset, int count) {
			if (_streamMode != LZ4StreamMode.Read) { throw new NotSupportedException("Read"); }
			else if (buffer == null) { throw new ArgumentNullException("buffer"); }
			else if (offset < 0) { throw new ArgumentOutOfRangeException("offset"); }
			else if (count < 0) { throw new ArgumentOutOfRangeException("count"); }
			else if (offset + count > buffer.Length) { throw new ArgumentOutOfRangeException("offset+count"); }

			if (_compressionMode == CompressionMode.Decompress) {
				int total = 0;
				while (count > 0) {
					int chunk = Math.Min(count, _outputBufferBlockSize - _outputBufferOffset);
					if (chunk > 0) {
						Buffer.BlockCopy(_outputBufferRef, _ringbufferOffset + _outputBufferOffset, buffer, offset, chunk);

						_outputBufferOffset += chunk;
						offset += chunk;
						count -= chunk;
						total += chunk;
						if (_interactiveRead) {
							break;
						}
					}
					else {
						if (!AcquireNextBlock()) break;
					}
				}
				return total;
			}
			else {
				return CompressData(buffer, offset, count);
			}
		}

		public override void WriteByte(byte value) {
			if (_streamMode != LZ4StreamMode.Write) { throw new NotSupportedException("Write"); }

			if (_compressionMode == CompressionMode.Compress) {
				if (!_hasWrittenStartFrame) { WriteStartFrame(); }

				if (_inputBufferOffset >= _inputBufferSize) {
					FlushCurrentBlock(false);
				}

				_inputBufferRef[_ringbufferOffset + _inputBufferOffset++] = value;
			}
			else {
				byte[] b = new byte[1];
				b[0] = value;
				DecompressData(b, 0, 1);
			}
		}

		public override void Write(byte[] buffer, int offset, int count) {
			if (_streamMode != LZ4StreamMode.Write) { throw new NotSupportedException("Write"); }
			else if (buffer == null) { throw new ArgumentNullException("buffer"); }
			else if (offset < 0) { throw new ArgumentOutOfRangeException("offset"); }
			else if (count < 0) { throw new ArgumentOutOfRangeException("count"); }
			else if (offset + count > buffer.Length) { throw new ArgumentOutOfRangeException("offset+count"); }

			if (count == 0) { return; }

			if (_compressionMode == CompressionMode.Compress) {
				if (!_hasWrittenStartFrame) { WriteStartFrame(); }

				while (count > 0) {
					int chunk = Math.Min(count, _inputBufferSize - _inputBufferOffset);
					if (chunk > 0) {
						Buffer.BlockCopy(buffer, offset, _inputBufferRef, _ringbufferOffset + _inputBufferOffset, chunk);

						offset += chunk;
						count -= chunk;
						_inputBufferOffset += chunk;
					}
					else {
						FlushCurrentBlock(false);
					}
				}
			}
			else {
				DecompressData(buffer, offset, count);
			}
		}

		private void DecompressData(byte[] data, int offset, int count) {
			int consumed = 0;
			do {
				offset += consumed;
				count -= consumed;
				consumed = 0;

				if (_currentMode >= 0 && _currentMode <= 4) {
					consumed = DecompressHeader(data, offset, count);
				}
				else if (_currentMode == 5) {
					// user data frame
					int l = _outputBufferSize;
					int remaining = l - _outputBufferOffset;
					int chunk = consumed = Math.Min(remaining, count);
					Buffer.BlockCopy(data, offset, _outputBufferRef, _outputBufferOffset, chunk);
					_outputBufferOffset += chunk;
					remaining = l - _outputBufferOffset;
					if (remaining == 0) {
						int id = (_headerBuffer[0] & 0xF);

						byte[] userData = new byte[l];
						Buffer.BlockCopy(_outputBufferRef, 0, userData, 0, l);
						var handler = UserDataFrameRead;
						if (handler != null) {
							LZ4UserDataFrameEventArgs e = new LZ4UserDataFrameEventArgs(id, userData);
							handler(this, e);
						}

						_headerBufferSize = 0;
						_currentMode = 0;
					}
				}
				else if (_currentMode >= 6 && _currentMode <= 10) {
					// lz4 frame
					consumed = DecompressBlock(data, offset, count);
				}
				else {
					throw new Exception("should not have happend, Write(): decompress, _currentmode == " + _currentMode);
				}
			} while (consumed < count);
		}

		private unsafe int DecompressBlock(byte[] data, int offset, int count) {
			int consumed = 0;
			if (_currentMode == 6) {
				for (int i = _headerBufferSize, ii = 0; i < 4 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 4) {

					_isCompressed = true;
					if ((_headerBuffer[3] & 0x80) == 0x80) {
						_isCompressed = false;
						_headerBuffer[3] &= 0x7F;
					}

					uint blockSize = 0;
					for (int i = 3; i >= 0; i--) {
						blockSize |= ((uint)_headerBuffer[i] << (i * 8));
					}

					if (blockSize > (uint)_outputBufferSize) {
						throw new Exception("Block size exceeds maximum block size");
					}

					if (blockSize == 0) {
						// end marker

						if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
							_headerBufferSize = 0;
							_currentMode = 7;
						}
						else {
							_headerBufferSize = 0;
							_currentMode = 0;
						}
						return consumed;
					}

					_targetBufferSize = (int)blockSize;
					_inputBufferOffset = 0;
					_currentMode = 8;
				}
			}
			else if (_currentMode == 7) {
				for (int i = _headerBufferSize, ii = 0; i < 4 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 4) {

					// calculate hash
					uint xxh = lz4.XXH32_digest(_unsafeData._contentHashState);

					if (_headerBuffer[0] != (byte)(xxh & 0xFF) ||
						_headerBuffer[1] != (byte)((xxh >> 8) & 0xFF) ||
						_headerBuffer[2] != (byte)((xxh >> 16) & 0xFF) ||
						_headerBuffer[3] != (byte)((xxh >> 24) & 0xFF)) {
						throw new Exception("Content checksum did not match");
					}

					_headerBufferSize = 0;
					_currentMode = 0;
				}
			}
			else if (_currentMode == 8) {

				int remaining = _targetBufferSize - _inputBufferOffset;

				int chunk = consumed = Math.Min(remaining, count);
				Buffer.BlockCopy(data, offset, _inputBufferRef, _inputBufferOffset, chunk);
				_inputBufferOffset += chunk;
				remaining = _targetBufferSize - _inputBufferOffset;
				if (remaining == 0) {

					_blockCount++;

					if ((_checksumMode & LZ4FrameChecksumMode.Block) == LZ4FrameChecksumMode.Block) {
						_headerBufferSize = 0;
						_currentMode = 9;
					}
					else {
						_currentMode = 10;
					}
				}
			}
			else if (_currentMode == 9) {
				// block checksum
				for (int i = _headerBufferSize, ii = 0; i < 4 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 4) {
					uint checksum = 0;
					for (int i = 3; i >= 0; i--) {
						checksum |= ((uint)_headerBuffer[i] << (i * 8));
					}
					// verify checksum
					void* targetPtr = &_unsafeData._inputBufferPtr[0];
					uint xxh = lz4.XXH32(targetPtr, (uint)_targetBufferSize, 0);
					if (checksum != xxh) {
						throw new Exception("Block checksum did not match");
					}

					_currentMode = 10;
				}
			}
			else if (_currentMode == 10) {
				int currentRingbufferOffset = _ringbufferOffset;

				// preserve previously compressed block data (for LZ4_decompress_safe_continue dictionary [LZ4FrameBlockMode.Linked])
				// update ringbuffer offset
				_ringbufferOffset += _outputBufferSize;
				// wraparound the ringbuffer offset
				if (_ringbufferOffset > _outputBufferSize) _ringbufferOffset = 0;

				if (!_isCompressed) {
					Unsafe.CopyBlock(&_unsafeData._outputBufferPtr[_ringbufferOffset], _unsafeData._inputBufferPtr, (uint)_targetBufferSize);
					_outputBufferBlockSize = _targetBufferSize;
				}
				else {
					byte* inputPtr = &_unsafeData._inputBufferPtr[0];
					byte* outputPtr = &_unsafeData._outputBufferPtr[_ringbufferOffset];
					byte* dict = &_unsafeData._outputBufferPtr[currentRingbufferOffset];
					int status;
					if (_blockCount > 1) {
						status = lz4.LZ4_setStreamDecode(_unsafeData._lz4DecodeStream, dict, _outputBufferSize);
					}
					else {
						status = lz4.LZ4_setStreamDecode(_unsafeData._lz4DecodeStream, null, 0);
					}
					if (status != 1) {
						throw new Exception("LZ4_setStreamDecode failed");
					}
					int decompressedSize = lz4.LZ4_decompress_safe_continue(_unsafeData._lz4DecodeStream, inputPtr, outputPtr, _targetBufferSize, _outputBufferSize);
					if (decompressedSize <= 0) {
						throw new Exception("Decompress failed");
					}
					_outputBufferBlockSize = decompressedSize;
				}

				_innerStream.Write(_outputBufferRef, _ringbufferOffset, _outputBufferBlockSize);

				if ((_checksumMode & LZ4FrameChecksumMode.Content) == LZ4FrameChecksumMode.Content) {
					void* contentPtr = &_unsafeData._outputBufferPtr[_ringbufferOffset];
					XXH_errorcode status = lz4.XXH32_update(_unsafeData._contentHashState, contentPtr, (uint)_outputBufferBlockSize);
					if (status != XXH_errorcode.XXH_OK) {
						throw new Exception("Failed to update content checksum");
					}
				}

				_headerBufferSize = 0;
				_outputBufferOffset = 0;
				_currentMode = 6;
			}
			else {
				throw new Exception("should not have happend, Write(): decompress, _currentmode == " + _currentMode);
			}

			return consumed;
		}

		private unsafe int DecompressHeader(byte[] data, int offset, int count) {
			// _currentMode == 0 . no data
			// _currentMode == 1 . lz4 frame magic
			// _currentMode == 2 . user frame magic
			// _currentMode == 3 . checksum
			// _currentMode == 4 . lz4 content size
			// _currentMode == 5 . user frame size		
			// _currentMode == 6 . frame data
			// _currentMode == 7 . content checksum
			// _currentMode == 8 . block data
			// _currentMode == 9 . block checksum
			// _currentMode == 10 . decompress block

			int consumed = 0;
			if (_currentMode == 0) {
				for (int i = _headerBufferSize, ii = 0; i < 4 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 4) {
					_blockCount = 0;
					if (_headerBuffer[0] == 0x04 && _headerBuffer[1] == 0x22 && _headerBuffer[2] == 0x4D && _headerBuffer[3] == 0x18) {
						// expect 2 byte descriptor
						_currentMode = 1;
					}
					else if (_headerBuffer[0] >= 0x50 && _headerBuffer[0] <= 0x5f && _headerBuffer[1] == 0x2A && _headerBuffer[2] == 0x4D && _headerBuffer[3] == 0x18) {
						_currentMode = 2;
					}
					else {
						throw new Exception("Invalid magic: 0x" + _headerBuffer[0].ToString("x2") + _headerBuffer[1].ToString("x2") + _headerBuffer[2].ToString("x2") + _headerBuffer[3].ToString("x2"));
					}
				}
			}
			else if (_currentMode == 1) {
				for (int i = _headerBufferSize, ii = 0; i < 6 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 6) {
					_frameCount++;

					// reset state
					lz4.XXH32_reset(_unsafeData._contentHashState, 0);
					_blockSize = LZ4FrameBlockSize.Max64KB;
					_blockMode = LZ4FrameBlockMode.Linked;
					_checksumMode = LZ4FrameChecksumMode.None;
					_contentSize = 0;
					_outputBufferOffset = 0;
					_outputBufferBlockSize = 0;
					//_ringbufferOffset = 0;

					// verify version
					if (!((_headerBuffer[4] & 0x40) == 0x40 || (_headerBuffer[4] & 0x60) == 0x60) || (_headerBuffer[4] & 0x80) != 0x00) {
						throw new Exception("Unexpected frame version");
					}
					else if ((_headerBuffer[4] & 0x01) != 0x00) {
						throw new Exception("Predefined dictionaries are not supported");
					}
					else if ((_headerBuffer[4] & 0x02) != 0x00) {
						// reserved value
						throw new Exception("Header contains unexpected value");
					}

					if ((_headerBuffer[4] & 0x04) == 0x04) {
						_checksumMode = _checksumMode | LZ4FrameChecksumMode.Content;
					}
					bool hasContentSize = false;
					if ((_headerBuffer[4] & 0x08) == 0x08) {
						hasContentSize = true;
					}
					if ((_headerBuffer[4] & 0x10) == 0x10) {
						_checksumMode = _checksumMode | LZ4FrameChecksumMode.Block;
					}
					if ((_headerBuffer[4] & 0x20) == 0x20) {
						_blockMode = LZ4FrameBlockMode.Independent;
					}

					if ((_headerBuffer[5] & 0x0F) != 0x00) {
						// reserved value
						throw new Exception("Header contains unexpected value");
					}
					else if ((_headerBuffer[5] & 0x80) != 0x00) {
						// reserved value
						throw new Exception("Header contains unexpected value");
					}

					int blockSizeId = (_headerBuffer[5] & 0x70) >> 4;
					if (blockSizeId == 4) {
						_blockSize = LZ4FrameBlockSize.Max64KB;
						_inputBufferSize = 64 * (1 << 10);
						_outputBufferSize = 64 * (1 << 10);
					}
					else if (blockSizeId == 5) {
						_blockSize = LZ4FrameBlockSize.Max256KB;
						_inputBufferSize = 256 * (1 << 10);
						_outputBufferSize = 256 * (1 << 10);
					}
					else if (blockSizeId == 6) {
						_blockSize = LZ4FrameBlockSize.Max1MB;
						_inputBufferSize = 1 * (1 << 20);
						_outputBufferSize = 1 * (1 << 20);
					}
					else if (blockSizeId == 7) {
						_blockSize = LZ4FrameBlockSize.Max4MB;
						_inputBufferSize = 4 * (1 << 20);
						_outputBufferSize = 4 * (1 << 20);
					}
					else {
						throw new Exception("Unsupported block size: " + blockSizeId);
					}

					// resize buffers
					if (_inputBufferRef == null || _inputBufferRef.Length < _inputBufferSize) {
						if (_inputBufferHandle.IsAllocated) { _inputBufferHandle.Free(); _inputBufferHandle = default; _unsafeData._inputBufferPtr = null; }
						if (_inputBufferRef != null) { ArrayPool<byte>.Shared.Return(_inputBufferRef); _inputBufferRef = null; }
						_inputBufferRef = ArrayPool<byte>.Shared.Rent(_inputBufferSize);
						_inputBufferHandle = GCHandle.Alloc(_inputBufferRef, GCHandleType.Pinned);
						_unsafeData._inputBufferPtr = (byte*)(void*)_inputBufferHandle.AddrOfPinnedObject();
					}
					if (_outputBufferRef == null || _outputBufferRef.Length < 2 * _outputBufferSize) {
						if (_outputBufferHandle.IsAllocated) { _outputBufferHandle.Free(); _outputBufferHandle = default; _unsafeData._outputBufferPtr = null; }
						if (_outputBufferRef != null) { ArrayPool<byte>.Shared.Return(_outputBufferRef); _outputBufferRef = null; }
						_outputBufferRef = ArrayPool<byte>.Shared.Rent(2 * _outputBufferSize);
						_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
						_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
					}

					if (hasContentSize) {
						// expect 8 more bytes
						_currentMode = 4;
					}
					else {
						_currentMode = 3;
					}
				}
			}
			else if (_currentMode == 4) {
				for (int i = _headerBufferSize, ii = 0; i < 8 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 8) {

					_contentSize = 0;
					for (int i = 9; i >= 2; i--) {
						_contentSize |= ((ulong)_headerBuffer[i] << (i * 8));
					}

					_currentMode = 3;
				}
			}
			else if (_currentMode == 3) {
				_headerBuffer[_headerBufferSize++] = data[offset];

				// verify checksum
				uint xxh;
				fixed (byte* descriptorPtr = &_headerBuffer[4]) {
					xxh = lz4.XXH32(descriptorPtr, (uint)_headerBufferSize - 5, 0);
				}
				byte checksumByte = (byte)((xxh >> 8) & 0xFF);
				if (_headerBuffer[_headerBufferSize - 1] != checksumByte) {
					throw new Exception("Frame checksum is invalid");
				}

				_headerBufferSize = 0;
				_currentMode = 6;
				consumed = 1;
			}
			else if (_currentMode == 2) {
				for (int i = _headerBufferSize, ii = 0; i < 8 && ii < count; i++, ii++) {
					_headerBuffer[i] = data[offset + ii];
					_headerBufferSize++;
					consumed++;
				}

				if (_headerBufferSize == 8) {

					_frameCount++;

					uint frameSize = 0;
					for (int i = _headerBufferSize - 1; i >= 4; i--) {
						frameSize |= ((uint)_headerBuffer[i] << (i * 8));
					}

					_outputBufferSize = (int)frameSize;
					_outputBufferOffset = 0;
					if (_outputBufferHandle.IsAllocated) { _outputBufferHandle.Free(); _outputBufferHandle = default; _unsafeData._outputBufferPtr = null; }
					if (_outputBufferRef != null) { ArrayPool<byte>.Shared.Return(_outputBufferRef); _outputBufferRef = null; }
					_outputBufferRef = ArrayPool<byte>.Shared.Rent(_outputBufferSize);
					_outputBufferHandle = GCHandle.Alloc(_outputBufferRef, GCHandleType.Pinned);
					_unsafeData._outputBufferPtr = (byte*)(void*)_outputBufferHandle.AddrOfPinnedObject();
					_currentMode = 5;
				}
			}

			return consumed;
		}
	}
}
