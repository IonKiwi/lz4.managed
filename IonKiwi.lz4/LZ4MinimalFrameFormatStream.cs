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
using System.Threading;
using System.Threading.Tasks;

#if NETSTANDARD2_0
using PlatformTask = System.Threading.Tasks.Task;
using PlatformTaskBool = System.Threading.Tasks.Task<bool>;
using PlatformTaskInt = System.Threading.Tasks.Task<int>;
#else
using PlatformTask = System.Threading.Tasks.ValueTask;
using PlatformTaskBool = System.Threading.Tasks.ValueTask<bool>;
#endif

namespace IonKiwi.lz4 {
	// based on blockStreaming_ringBuffer.c
	public sealed class LZ4MinimalFrameFormatStream : Stream {

		private bool _disposed;
		private Stream _innerStream;

		private bool _interactiveRead = false;
		private CompressionMode _compressionMode;
		private bool _leaveInnerStreamOpen;
		private int _blockSize;
		private int _ringbufferSlots;
		private int _ringbufferSize;
		private byte[] _ringbuffer;
		private int _inputBufferOffset;
		private int _ringbufferOffset;
		private int _inputBufferLength;
		private GCHandle _ringbufferHandle;
		private IDisposable _lz4StreamDispose = null;
		private IDisposable _lz4DecodeStreamDispose = null;

		private UnsafeData _unsafeData = new UnsafeData();

		private sealed unsafe class UnsafeData {
			public byte* _ringbufferPtr;
			public LZ4_stream* _lz4Stream = null;
			public LZ4_streamDecode* _lz4DecodeStream = null;
		}

		public LZ4MinimalFrameFormatStream(Stream innerStream, CompressionMode compressionMode, bool leaveInnerStreamOpen = true)
			: this(innerStream, compressionMode, 64 * (1 << 10), leaveInnerStreamOpen) {

		}

		public LZ4MinimalFrameFormatStream(Stream innerStream, CompressionMode compressionMode, int blockSize, bool leaveInnerStreamOpen = true) {
			if (innerStream == null) {
				throw new ArgumentNullException(nameof(innerStream));
			}
			else if (blockSize < 1) {
				throw new ArgumentOutOfRangeException(nameof(blockSize));
			}

			_innerStream = innerStream;
			_compressionMode = compressionMode;
			_leaveInnerStreamOpen = leaveInnerStreamOpen;
			_blockSize = blockSize;

			int ringbufferSlots = 2; // create linked blocks (up to LZ4 window size, which is 64 KB)
			if (blockSize < 64 * (1 << 10)) {
				ringbufferSlots = (int)Math.Ceiling((double)(2 * 64 * (1 << 10)) / blockSize);
			}
			_ringbufferSlots = ringbufferSlots;
			Init();
		}

		public bool InteractiveRead {
			get => _interactiveRead;
			set => _interactiveRead = value;
		}

		protected override void Dispose(bool disposing) {
			try {
				if (disposing) {
					if (!_disposed) {
						Flush();
						if (!_leaveInnerStreamOpen) {
							_innerStream.Dispose();
						}
						_disposed = true;
					}
				}
				// NOTE: do NOT call base.Dispose, base impl is empty
				//base.Dispose(disposing);
			}
			finally {
				FreeState();
			}
		}

#if NETSTANDARD2_1
		public override async ValueTask DisposeAsync() {
			try {
				if (!_disposed) {
					await FlushAsync().ConfigureAwait(false);
					if (!_leaveInnerStreamOpen) {
						await _innerStream.DisposeAsync().ConfigureAwait(false);
					}
					_disposed = true;
				}
				// NOTE: do NOT call base.DisposeAsync() as it will call Dispose()
				//await base.DisposeAsync().ConfigureAwait(false);
			}
			finally {
				FreeState();
			}
		}
#endif

		private unsafe void FreeState() {
			if (_ringbufferHandle.IsAllocated) { _ringbufferHandle.Free(); _ringbufferHandle = default; _unsafeData._ringbufferPtr = null; }

			if (_ringbuffer != null) { ArrayPool<byte>.Shared.Return(_ringbuffer); _ringbuffer = null; }

			if (_lz4StreamDispose != null) { _lz4StreamDispose.Dispose(); _lz4StreamDispose = null; }
			if (_lz4DecodeStreamDispose != null) { _lz4DecodeStreamDispose.Dispose(); _lz4DecodeStreamDispose = null; }
		}

		private unsafe void Init() {
			_ringbufferSize = _ringbufferSlots * _blockSize; // +_blockSize;
			_ringbuffer = ArrayPool<byte>.Shared.Rent(_ringbufferSize);
			_ringbufferHandle = GCHandle.Alloc(_ringbuffer, GCHandleType.Pinned);
			_unsafeData._ringbufferPtr = (byte*)(void*)_ringbufferHandle.AddrOfPinnedObject();

			if (_compressionMode == CompressionMode.Compress) {
				_unsafeData._lz4Stream = lz4.LZ4_createStream(out _lz4StreamDispose);
			}
			else {
				_unsafeData._lz4DecodeStream = lz4.LZ4_createStreamDecode(out _lz4DecodeStreamDispose);
			}
		}

		public override bool CanRead => _compressionMode == CompressionMode.Decompress && !_disposed;

		public override bool CanSeek => false;

		public override bool CanWrite => _compressionMode == CompressionMode.Compress && !_disposed;

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
			CheckDisposed();
			if (_inputBufferOffset > 0 && CanWrite) { FlushCurrentChunk(); };
		}

		public override async Task FlushAsync(CancellationToken cancellationToken) {
			CheckDisposed();
			if (_inputBufferOffset > 0 && CanWrite) { await FlushCurrentChunkAsync().ConfigureAwait(false); };
		}

		private void CheckDisposed() {
			if (_disposed) {
				throw new ObjectDisposedException(nameof(LZ4Stream));
			}
		}

		private void FlushCurrentChunk() {
			if (_inputBufferOffset <= 0) { return; }

			if (_ringbufferSlots == 1) {
				// reset the stream { create independently compressed blocks }
				ResetStream();
			}
			int maxLength = lz4.LZ4_compressBound(_inputBufferOffset);
			byte[] outputBuffer = ArrayPool<byte>.Shared.Rent(maxLength);
			try {
				int outputBytes = CompressInternal(outputBuffer, maxLength);
				if (outputBytes <= 0) {
					throw new Exception("Compress failed");
				}

				byte[] b = new byte[4];
				b[0] = (byte)((uint)outputBytes & 0xFF);
				b[1] = (byte)(((uint)outputBytes >> 8) & 0xFF);
				b[2] = (byte)(((uint)outputBytes >> 16) & 0xFF);
				b[3] = (byte)(((uint)outputBytes >> 24) & 0xFF);

				_innerStream.Write(b, 0, b.Length);
				_innerStream.Write(outputBuffer, 0, outputBytes);
			}
			finally {
				ArrayPool<byte>.Shared.Return(outputBuffer);
			}

			// update ringbuffer offset
			_ringbufferOffset += _inputBufferOffset;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _ringbufferSize - _blockSize) _ringbufferOffset = 0;
			// reset input offset
			_inputBufferOffset = 0;
		}

		private async PlatformTask FlushCurrentChunkAsync() {
			if (_inputBufferOffset <= 0) { return; }

			if (_ringbufferSlots == 1) {
				// reset the stream { create independently compressed blocks }
				ResetStream();
			}

			int maxLength = lz4.LZ4_compressBound(_inputBufferOffset);
			byte[] outputBuffer = ArrayPool<byte>.Shared.Rent(maxLength);
			try {
				int outputBytes = CompressInternal(outputBuffer, maxLength);
				if (outputBytes <= 0) {
					throw new Exception("Compress failed");
				}

				byte[] b = new byte[4];
				b[0] = (byte)((uint)outputBytes & 0xFF);
				b[1] = (byte)(((uint)outputBytes >> 8) & 0xFF);
				b[2] = (byte)(((uint)outputBytes >> 16) & 0xFF);
				b[3] = (byte)(((uint)outputBytes >> 24) & 0xFF);

				await _innerStream.WriteAsync(b, 0, b.Length).ConfigureAwait(false);
				await _innerStream.WriteAsync(outputBuffer, 0, outputBytes).ConfigureAwait(false);
			}
			finally {
				ArrayPool<byte>.Shared.Return(outputBuffer);
			}

			// update ringbuffer offset
			_ringbufferOffset += _inputBufferOffset;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _ringbufferSize - _blockSize) _ringbufferOffset = 0;
			// reset input offset
			_inputBufferOffset = 0;
		}

		private unsafe void ResetStream() {
			lz4.LZ4_loadDict(_unsafeData._lz4Stream, null, 0);
		}

		private unsafe int CompressInternal(byte[] outputBuffer, int maxLength) {
			fixed (byte* outputBufferPtr = &outputBuffer[0]) {
				return lz4.LZ4_compress_fast_continue(_unsafeData._lz4Stream, &_unsafeData._ringbufferPtr[_ringbufferOffset], outputBufferPtr, _inputBufferOffset, maxLength, 1);
			}
		}

		private bool AcquireNextChunk() {
			// read chunk size
			byte[] sizeBuffer = new byte[4];
			int bytesRead = _innerStream.Read(sizeBuffer, 0, sizeBuffer.Length);
			if (bytesRead == 0) { return false; }
			else if (bytesRead != sizeBuffer.Length) { throw new EndOfStreamException("Unexpected end of stream"); }
			else if ((sizeBuffer[sizeBuffer.Length - 1] & 0x80) == 0x80) { throw new Exception("Invalid data"); }

			int sizeValue = 0;
			for (int i = sizeBuffer.Length - 1; i >= 0; i--) {
				sizeValue |= ((int)sizeBuffer[i] << (i * 8));
			}

			if (sizeValue == 0) {
				return false;
			}

			// update ringbuffer offset
			_ringbufferOffset += _inputBufferLength;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _ringbufferSize - _blockSize) _ringbufferOffset = 0;

			// read chunk data
			int outputBytes;
			byte[] chunkData = ArrayPool<byte>.Shared.Rent(sizeValue);
			try {
				bytesRead = _innerStream.Read(chunkData, 0, sizeValue);
				if (bytesRead != sizeValue) { throw new EndOfStreamException("Unexpected end of stream"); }

				outputBytes = DecompressInternal(chunkData, sizeValue);
			}
			finally {
				ArrayPool<byte>.Shared.Return(chunkData);
			}
			if (outputBytes <= 0) {
				throw new Exception("Decompress failed");
			}

			// set the input offset
			_inputBufferLength = outputBytes;
			_inputBufferOffset = 0;
			return true;
		}

		private async PlatformTaskBool AcquireNextChunkAsync() {
			// read chunk size
			byte[] sizeBuffer = new byte[4];
			int bytesRead = await _innerStream.ReadAsync(sizeBuffer, 0, sizeBuffer.Length).ConfigureAwait(false);
			if (bytesRead == 0) { return false; }
			else if (bytesRead != sizeBuffer.Length) { throw new EndOfStreamException("Unexpected end of stream"); }
			else if ((sizeBuffer[sizeBuffer.Length - 1] & 0x80) == 0x80) { throw new Exception("Invalid data"); }

			int sizeValue = 0;
			for (int i = sizeBuffer.Length - 1; i >= 0; i--) {
				sizeValue |= ((int)sizeBuffer[i] << (i * 8));
			}

			if (sizeValue == 0) {
				return false;
			}

			// update ringbuffer offset
			_ringbufferOffset += _inputBufferLength;
			// wraparound the ringbuffer offset
			if (_ringbufferOffset > _ringbufferSize - _blockSize) _ringbufferOffset = 0;

			// read chunk data
			int outputBytes;
			byte[] chunkData = ArrayPool<byte>.Shared.Rent(sizeValue);
			try {
				bytesRead = await _innerStream.ReadAsync(chunkData, 0, sizeValue).ConfigureAwait(false);
				if (bytesRead != sizeValue) { throw new EndOfStreamException("Unexpected end of stream"); }

				outputBytes = DecompressInternal(chunkData, sizeValue);
			}
			finally {
				ArrayPool<byte>.Shared.Return(chunkData);
			}
			if (outputBytes <= 0) {
				throw new Exception("Decompress failed");
			}

			// set the input offset
			_inputBufferLength = outputBytes;
			_inputBufferOffset = 0;
			return true;
		}

		private unsafe int DecompressInternal(byte[] chunkData, int sizeValue) {
			fixed (byte* inputBufferPtr = &chunkData[0]) {
				return lz4.LZ4_decompress_safe_continue(_unsafeData._lz4DecodeStream, inputBufferPtr, &_unsafeData._ringbufferPtr[_ringbufferOffset], sizeValue, _blockSize);
			}
		}

		public override int ReadByte() {
			CheckDisposed();
			if (!CanRead) { throw new NotSupportedException(nameof(ReadByte)); }

			if (_inputBufferOffset >= _inputBufferLength && !AcquireNextChunk())
				return -1; // that's just end of stream
			return _ringbuffer[_ringbufferOffset + _inputBufferOffset++];
		}

		public override int Read(byte[] buffer, int offset, int count) {
			CheckDisposed();
			if (!CanRead) { throw new NotSupportedException(nameof(Read)); }

			int total = 0;
			while (count > 0) {
				int chunk = Math.Min(count, _inputBufferLength - _inputBufferOffset);
				if (chunk > 0) {
					Buffer.BlockCopy(_ringbuffer, _ringbufferOffset + _inputBufferOffset, buffer, offset, chunk);

					_inputBufferOffset += chunk;
					offset += chunk;
					count -= chunk;
					total += chunk;
					if (_interactiveRead) {
						break;
					}
				}
				else {
					if (!AcquireNextChunk()) break;
				}
			}
			return total;
		}

		public override async Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken) {
			CheckDisposed();
			if (!CanRead) { throw new NotSupportedException(nameof(Read)); }

			int total = 0;
			while (count > 0) {
				int chunk = Math.Min(count, _inputBufferLength - _inputBufferOffset);
				if (chunk > 0) {
					Buffer.BlockCopy(_ringbuffer, _ringbufferOffset + _inputBufferOffset, buffer, offset, chunk);

					_inputBufferOffset += chunk;
					offset += chunk;
					count -= chunk;
					total += chunk;
					if (_interactiveRead) {
						break;
					}
				}
				else {
					if (!await AcquireNextChunkAsync().ConfigureAwait(false)) break;
				}
			}
			return total;
		}

		public override IAsyncResult BeginRead(byte[] buffer, int offset, int count, AsyncCallback callback, object state) {
			CheckDisposed();
			return AsApm(ReadAsync(buffer, offset, count), callback, state);
		}

		public override int EndRead(IAsyncResult asyncResult) {
			CheckDisposed();
			if (asyncResult == null) {
				throw new ArgumentNullException(nameof(asyncResult));
			}
			var task = asyncResult as Task<int>;
			if (task == null) {
				throw new Exception($"Expected '{nameof(asyncResult)}' to be of type 'Task<int>'.");
			}
			return task.GetAwaiter().GetResult();
		}

		public override void WriteByte(byte value) {
			CheckDisposed();
			if (!CanWrite) { throw new NotSupportedException(nameof(WriteByte)); }

			if (_inputBufferOffset >= _blockSize) {
				FlushCurrentChunk();
			}

			_ringbuffer[_ringbufferOffset + _inputBufferOffset++] = value;
		}

		public override void Write(byte[] buffer, int offset, int count) {
			CheckDisposed();
			if (!CanWrite) { throw new NotSupportedException(nameof(Write)); }

			while (count > 0) {
				int chunk = Math.Min(count, _blockSize - _inputBufferOffset);
				if (chunk > 0) {
					// write data to ringbuffer
					Buffer.BlockCopy(buffer, offset, _ringbuffer, _ringbufferOffset + _inputBufferOffset, chunk);

					offset += chunk;
					count -= chunk;
					_inputBufferOffset += chunk;
				}
				else {
					FlushCurrentChunk();
				}
			}
		}

		public override async Task WriteAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken) {
			CheckDisposed();
			if (!CanWrite) { throw new NotSupportedException(nameof(Write)); }

			while (count > 0) {
				int chunk = Math.Min(count, _blockSize - _inputBufferOffset);
				if (chunk > 0) {
					// write data to ringbuffer
					Buffer.BlockCopy(buffer, offset, _ringbuffer, _ringbufferOffset + _inputBufferOffset, chunk);

					offset += chunk;
					count -= chunk;
					_inputBufferOffset += chunk;
				}
				else {
					await FlushCurrentChunkAsync().ConfigureAwait(false);
				}
			}
		}

		public override IAsyncResult BeginWrite(byte[] buffer, int offset, int count, AsyncCallback callback, object state) {
			CheckDisposed();
			return AsApm(WriteAsync(buffer, offset, count), callback, state);
		}

		public override void EndWrite(IAsyncResult asyncResult) {
			CheckDisposed();
			if (asyncResult == null) {
				throw new ArgumentNullException(nameof(asyncResult));
			}
			var task = asyncResult as Task<object>;
			if (task == null) {
				throw new Exception($"Expected '{nameof(asyncResult)}' to be of type 'Task<object>'.");
			}
			task.GetAwaiter().GetResult();
		}

		private static IAsyncResult AsApm<T>(Task<T> task, AsyncCallback callback, object state) {
			if (task == null) {
				throw new ArgumentNullException(nameof(task));
			}
			var tcs = new TaskCompletionSource<T>(state);
			task.ContinueWith(t => {
				if (t.IsFaulted) {
					tcs.TrySetException(t.Exception.InnerExceptions);
				}
				else if (t.IsCanceled) {
					tcs.TrySetCanceled();
				}
				else {
					tcs.TrySetResult(t.Result);
				}
				if (callback != null) {
					callback(tcs.Task);
				}
			}, TaskScheduler.Default);
			return tcs.Task;
		}

		private static IAsyncResult AsApm(Task task, AsyncCallback callback, object state) {
			if (task == null) {
				throw new ArgumentNullException(nameof(task));
			}
			var tcs = new TaskCompletionSource<object>(state);
			task.ContinueWith(t => {
				if (t.IsFaulted) {
					tcs.TrySetException(t.Exception.InnerExceptions);
				}
				else if (t.IsCanceled) {
					tcs.TrySetCanceled();
				}
				else {
					tcs.TrySetResult(null);
				}
				if (callback != null) {
					callback(tcs.Task);
				}
			}, TaskScheduler.Default);
			return tcs.Task;
		}
	}
}
