using System;
using System.Collections.Generic;
using System.Text;

namespace IonKiwi.lz4 {
	public static class LZ4RawUtility {
		public static int CompressBound(int size) {
			return lz4.LZ4_compressBound(size);
		}

		public static unsafe int Compress(byte[] input, int inputOffset, int length, byte[] output, int outputOffset) {
			if (input == null) {
				throw new ArgumentNullException(nameof(input));
			}
			else if (inputOffset < 0 || inputOffset >= input.Length) {
				throw new ArgumentOutOfRangeException(nameof(inputOffset));
			}
			else if (length < 0 || inputOffset + length > input.Length) {
				throw new ArgumentOutOfRangeException(nameof(length));
			}
			else if (output == null) {
				throw new ArgumentNullException(nameof(output));
			}
			else if (outputOffset < 0 || outputOffset >= output.Length) {
				throw new ArgumentOutOfRangeException(nameof(outputOffset));
			}
			int maxLength = lz4.LZ4_compressBound(length - inputOffset);
			if (outputOffset + maxLength > output.Length) {
				throw new InvalidOperationException($"Output should be at least '{maxLength}' bytes (not including '{nameof(outputOffset)}').");
			}
			int compressedSize;
			fixed (byte* inputPtr = &input[inputOffset], outputPtr = &output[outputOffset]) {
				compressedSize = lz4.LZ4_compress_default(inputPtr, outputPtr, length, maxLength);
			}
			if (compressedSize <= 0) {
				throw new Exception("Compression failed");
			}
			return compressedSize;
		}

		public static unsafe int CompressHC(byte[] input, int inputOffset, int length, byte[] output, int outputOffset, int compressionLevel) {
			if (input == null) {
				throw new ArgumentNullException(nameof(input));
			}
			else if (inputOffset < 0 || inputOffset >= input.Length) {
				throw new ArgumentOutOfRangeException(nameof(inputOffset));
			}
			else if (length < 0 || inputOffset + length > input.Length) {
				throw new ArgumentOutOfRangeException(nameof(length));
			}
			else if (output == null) {
				throw new ArgumentNullException(nameof(output));
			}
			else if (outputOffset < 0 || outputOffset >= output.Length) {
				throw new ArgumentOutOfRangeException(nameof(outputOffset));
			}
			int maxLength = lz4.LZ4_compressBound(length - inputOffset);
			if (outputOffset + maxLength > output.Length) {
				throw new InvalidOperationException($"Output should be at least '{maxLength}' bytes (not including '{nameof(outputOffset)}').");
			}
			int compressedSize;
			fixed (byte* inputPtr = &input[inputOffset], outputPtr = &output[outputOffset]) {
				compressedSize = lz4.LZ4_compress_HC(inputPtr, outputPtr, length, maxLength, compressionLevel);
			}
			if (compressedSize <= 0) {
				throw new Exception("Compression failed");
			}
			return compressedSize;
		}

		public static unsafe int Decompress(byte[] input, int inputOffset, int length, byte[] output, int outputOffset) {
			if (input == null) {
				throw new ArgumentNullException(nameof(input));
			}
			else if (inputOffset < 0 || inputOffset >= input.Length) {
				throw new ArgumentOutOfRangeException(nameof(inputOffset));
			}
			else if (length < 0 || inputOffset + length > input.Length) {
				throw new ArgumentOutOfRangeException(nameof(length));
			}
			else if (output == null) {
				throw new ArgumentNullException(nameof(output));
			}
			else if (outputOffset < 0 || outputOffset >= output.Length) {
				throw new ArgumentOutOfRangeException(nameof(outputOffset));
			}

			int decompressedSize;
			fixed (byte* inputPtr = &input[inputOffset], outputPtr = &output[outputOffset]) {
				decompressedSize = lz4.LZ4_decompress_safe(inputPtr, outputPtr, length, output.Length - outputOffset);
			}
			if (decompressedSize <= 0) {
				throw new Exception("Decompression failed");
			}
			return decompressedSize;
		}
	}
}
