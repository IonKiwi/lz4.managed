using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace IonKiwi.lz4 {
	public static class LZ4Utility {
		public static byte[] Compress(byte[] input, LZ4FrameBlockMode blockMode = LZ4FrameBlockMode.Linked, LZ4FrameBlockSize blockSize = LZ4FrameBlockSize.Max1MB, LZ4FrameChecksumMode checksumMode = LZ4FrameChecksumMode.Content, long? maxFrameSize = null, bool highCompression = false) {
			if (input == null) {
				throw new ArgumentNullException("input");
			}
			return Compress(input, 0, input.Length, blockMode, blockSize, checksumMode, maxFrameSize, highCompression);
		}

		public static byte[] Compress(byte[] input, int inputOffset, int inputLength, LZ4FrameBlockMode blockMode = LZ4FrameBlockMode.Linked, LZ4FrameBlockSize blockSize = LZ4FrameBlockSize.Max1MB, LZ4FrameChecksumMode checksumMode = LZ4FrameChecksumMode.Content, long? maxFrameSize = null, bool highCompression = false) {
			if (input == null) {
				throw new ArgumentNullException("input");
			}
			else if (inputOffset < 0) {
				throw new ArgumentOutOfRangeException("inputOffset");
			}
			else if (inputLength <= 0) {
				throw new ArgumentOutOfRangeException("inputLength");
			}
			else if (inputOffset + inputLength > input.Length) {
				throw new ArgumentOutOfRangeException("inputOffset+inputLength");
			}

			using (var ms = new MemoryStream()) {
				using (var lz4 = LZ4Stream.CreateCompressor(ms, LZ4StreamMode.Write, blockMode, blockSize, checksumMode, maxFrameSize, highCompression, true)) {
					lz4.Write(input, inputOffset, inputLength);
				}
				return ms.ToArray();
			}
		}

		public static byte[] Decompress(byte[] input) {
			if (input == null) {
				throw new ArgumentNullException("input");
			}
			return Decompress(input, 0, input.Length);
		}

		public static byte[] Decompress(byte[] input, int inputOffset, int inputLength) {
			if (input == null) {
				throw new ArgumentNullException("input");
			}
			else if (inputOffset < 0) {
				throw new ArgumentOutOfRangeException("inputOffset");
			}
			else if (inputLength < 3) {
				throw new ArgumentOutOfRangeException("inputLength");
			}
			else if (inputOffset + inputLength > input.Length) {
				throw new ArgumentOutOfRangeException("inputOffset+inputLength");
			}

			using (var ms = new MemoryStream()) {
				using (var lz4 = LZ4Stream.CreateDecompressor(ms, LZ4StreamMode.Write, true)) {
					lz4.Write(input, inputOffset, inputLength);
				}
				return ms.ToArray();
			}
		}
	}
}
