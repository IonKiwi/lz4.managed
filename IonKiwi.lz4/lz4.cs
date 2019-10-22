/*
   LZ4 - Fast LZ compression algorithm
   Copyright (C) 2011-present, Yann Collet.

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

   You can contact the original author at :
    - LZ4 homepage : http://www.lz4.org
    - LZ4 source repository : https://github.com/lz4/lz4

	 This is a translation of the original lz4 sources to C#
   lz4.c / lz4hc.c / xxhash.c
	  - source repository : https://github.com/IonKiwi/lz4.managed
*/

using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using size_t = System.UInt32;

namespace IonKiwi.lz4 {
	internal static class lz4 {
		private const int LZ4_MEMORY_USAGE = 14;
		private const int LZ4_HASHLOG = (LZ4_MEMORY_USAGE - 2);
		internal const int LZ4_HASH_SIZE_U32 = (1 << LZ4_HASHLOG);
		internal const int LZ4_STREAMSIZE_U64 = ((1 << (LZ4_MEMORY_USAGE - 3)) + 4);
		private const int LZ4HC_HASH_LOG = 15;
		internal const int LZ4HC_HASHTABLESIZE = (1 << LZ4HC_HASH_LOG);
		private const int LZ4HC_DICTIONARY_LOGSIZE = 16;
		internal const int LZ4HC_MAXD = (1 << LZ4HC_DICTIONARY_LOGSIZE);
		private const int LZ4_STREAMHCSIZE = (4 * LZ4HC_HASHTABLESIZE + 2 * LZ4HC_MAXD + 56);
		internal const int LZ4_STREAMHCSIZE_SIZET = (LZ4_STREAMHCSIZE / sizeof(size_t));
		internal const int LZ4_STREAMDECODESIZE_U64 = 4;

		private const int LZ4HC_CLEVEL_MIN = 3;
		private const int LZ4HC_CLEVEL_DEFAULT = 9;
		private const int LZ4HC_CLEVEL_OPT_MIN = 10;
		private const int LZ4HC_CLEVEL_MAX = 12;

		private const uint PRIME32_1 = 2654435761U;
		private const uint PRIME32_2 = 2246822519U;
		private const uint PRIME32_3 = 3266489917U;
		private const uint PRIME32_4 = 668265263U;
		private const uint PRIME32_5 = 374761393U;

		private static int HASH_UNIT = IntPtr.Size;

		private const int LZ4_DISTANCE_MAX = 65535;
		private const int ACCELERATION_DEFAULT = 1;

		private const int TRAILING_LITERALS = 3;
		private const int LZ4_OPT_NUM = (1 << 12);

		private const int WILDCOPYLENGTH = 8;
		private const int MATCH_SAFEGUARD_DISTANCE = ((2 * WILDCOPYLENGTH) - 4);   /* ensure it's possible to write 2 x wildcopyLength without overflowing output buffer */

		private const int MFLIMIT = 12;
		private const int LASTLITERALS = 5;
		private const uint LZ4_MAX_INPUT_SIZE = 0x7E000000;
		private const int LZ4_64Klimit = ((64 * (1 << 10)) + (MFLIMIT - 1));
		private const int LZ4_minLength = (MFLIMIT + 1);
		// NOTE: translation: changed uint to int
		private const int LZ4_skipTrigger = 6;
		private const int LZ4_DISTANCE_ABSOLUTE_MAX = 65535;

		internal static int[] inc32table = { 0, 1, 2, 1, 0, 4, 4, 4 };
		internal static int[] dec64table = { 0, 0, 0, -1, -4, 1, 2, 3 };

		private static uint[] DeBruijnBytePos64 = { 0, 0, 0, 0, 0, 1, 1, 2,
																										 0, 3, 1, 3, 1, 4, 2, 7,
																										 0, 2, 3, 6, 1, 5, 3, 5,
																										 1, 3, 4, 4, 2, 5, 6, 7,
																										 7, 0, 1, 2, 3, 3, 4, 6,
																										 2, 6, 5, 5, 3, 4, 5, 6,
																										 7, 1, 2, 4, 6, 4, 4, 5,
																										 7, 2, 6, 5, 7, 6, 7, 7 };
		private static uint[] DeBruijnBytePos32 = { 0, 0, 3, 0, 3, 1, 3, 0,
																										 3, 2, 2, 1, 3, 2, 0, 1,
																										 3, 3, 1, 2, 2, 2, 2, 0,
																										 3, 1, 2, 0, 1, 0, 1, 1 };

		private static cParams_t[] clTable = {
				new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=2, targetLength=16 },  /* 0, unused */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=2, targetLength=16 },  /* 1, unused */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=2, targetLength=16 },  /* 2, unused */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=4, targetLength=16 },  /* 3 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=8, targetLength=16 },  /* 4 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=16, targetLength=16 },  /* 5 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=32, targetLength=16 },  /* 6 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=64, targetLength=16 },  /* 7 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=128, targetLength=16 },  /* 8 */
        new cParams_t() { strat=lz4hc_strat_e.lz4hc, nbSearches=256, targetLength=16 },  /* 9 */
        new cParams_t() { strat=lz4hc_strat_e.lz4opt, nbSearches=96, targetLength=64 },  /*10==LZ4HC_CLEVEL_OPT_MIN*/
        new cParams_t() { strat=lz4hc_strat_e.lz4opt, nbSearches=512, targetLength=128 },  /*11 */
        new cParams_t() { strat=lz4hc_strat_e.lz4opt, nbSearches=16384, targetLength=LZ4_OPT_NUM },  /* 12==LZ4HC_CLEVEL_MAX */
    };

		//internal static unsafe int LZ4_freeStream(LZ4_stream_u* LZ4_stream) {
		//	if (LZ4_stream == null) return 0;   /* support free on null */
		//	//DEBUGLOG(5, "LZ4_freeStream %p", LZ4_stream);
		//	//FREEMEM(LZ4_stream);
		//	return (0);
		//}

		//internal static unsafe int LZ4_freeStreamHC(LZ4_streamHC_u* LZ4_streamHCPtr) {
		//	//DEBUGLOG(4, "LZ4_freeStreamHC(%p)", LZ4_streamHCPtr);
		//	if (LZ4_streamHCPtr == null) return 0;  /* support free on null */

		//	//FREEMEM(LZ4_streamHCPtr);
		//	return 0;
		//}

		//internal static unsafe XXH_errorcode XXH32_freeState(XXH32_state_s* statePtr) {
		//	//XXH_free(statePtr);
		//	return XXH_errorcode.XXH_OK;
		//}

		//internal static unsafe int LZ4_freeStreamDecode(LZ4_streamDecode_u* LZ4_stream) {
		//	if (LZ4_stream == null) { return 0; }  /* support free on null */
		//	//FREEMEM(LZ4_stream);
		//	return 0;
		//}

		private sealed class AllocWrapper : IDisposable {

			private IntPtr _handle;

			private AllocWrapper(IntPtr handle) {
				_handle = handle;
			}

			public static unsafe T* Alloc<T>(out IDisposable disposable) where T : unmanaged {
				var handle = Marshal.AllocHGlobal(sizeof(T));
				disposable = new AllocWrapper(handle);
				return (T*)handle;
			}

			public void Dispose() {
				Marshal.FreeHGlobal(_handle);
			}
		}

		internal static unsafe LZ4_stream* LZ4_createStream(out IDisposable disposable) {
			var lz4s = AllocWrapper.Alloc<LZ4_stream>(out disposable);
			//LZ4_STATIC_ASSERT(LZ4_STREAMSIZE >= sizeof(LZ4_stream_t_internal));    /* A compilation error here means LZ4_STREAMSIZE is not large enough */
			//DEBUGLOG(4, "LZ4_createStream %p", lz4s);
			if (lz4s == null) return null;
			LZ4_initStream(lz4s, (uint)sizeof(LZ4_stream));
			return lz4s;
		}

		internal static unsafe LZ4_streamHC* LZ4_createStreamHC(out IDisposable disposable) {
			var LZ4_streamHCPtr = AllocWrapper.Alloc<LZ4_streamHC>(out disposable);
			if (LZ4_streamHCPtr == null) return null;
			LZ4_initStreamHC(LZ4_streamHCPtr, (uint)sizeof(LZ4_streamHC));  /* full initialization, malloc'ed buffer can be full of garbage */
			return LZ4_streamHCPtr;
		}

		internal static unsafe LZ4_streamDecode* LZ4_createStreamDecode(out IDisposable disposable) {
			LZ4_streamDecode* lz4s = AllocWrapper.Alloc<LZ4_streamDecode>(out disposable);
			//LZ4_STATIC_ASSERT(LZ4_STREAMDECODESIZE >= sizeof(LZ4_streamDecode_t_internal));    /* A compilation error here means LZ4_STREAMDECODESIZE is not large enough */
			return lz4s;
		}

		internal static unsafe XXH32_state* XXH32_createState(out IDisposable disposable) {
			//return (XXH32_state_t*)XXH_malloc(sizeof(XXH32_state_t));
			return AllocWrapper.Alloc<XXH32_state>(out disposable);
		}

		private static unsafe LZ4_stream* LZ4_initStream(void* buffer, size_t size) {
			//DEBUGLOG(5, "LZ4_initStream");
			if (buffer == null) { return null; }
			if (size < sizeof(LZ4_stream)) { return null; }
			Unsafe.InitBlock(buffer, 0, (uint)sizeof(LZ4_stream));
			return (LZ4_stream*)buffer;
		}

		private static unsafe LZ4_streamHC* LZ4_initStreamHC(void* buffer, size_t size) {
			LZ4_streamHC* LZ4_streamHCPtr = (LZ4_streamHC*)buffer;
			if (buffer == null) return null;
			if (size < sizeof(LZ4_streamHC)) return null;
			/* if compilation fails here, LZ4_STREAMHCSIZE must be increased */
			//LZ4_STATIC_ASSERT(sizeof(LZ4HC_CCtx_internal) <= LZ4_STREAMHCSIZE);
			//DEBUGLOG(4, "LZ4_initStreamHC(%p, %u)", LZ4_streamHCPtr, (uint)size);
			/* end-base will trigger a clearTable on starting compression */
			LZ4_streamHCPtr->internal_donotuse.end = (byte*)-1;
			LZ4_streamHCPtr->internal_donotuse.baseAddr = null;
			LZ4_streamHCPtr->internal_donotuse.dictCtx = null;
			LZ4_streamHCPtr->internal_donotuse.favorDecSpeed = 0;
			LZ4_streamHCPtr->internal_donotuse.dirty = 0;
			LZ4_setCompressionLevel(LZ4_streamHCPtr, LZ4HC_CLEVEL_DEFAULT);
			return LZ4_streamHCPtr;
		}

		private static unsafe void LZ4_setCompressionLevel(LZ4_streamHC* LZ4_streamHCPtr, int compressionLevel) {
			//DEBUGLOG(5, "LZ4_setCompressionLevel(%p, %d)", LZ4_streamHCPtr, compressionLevel);
			if (compressionLevel < 1) compressionLevel = LZ4HC_CLEVEL_DEFAULT;
			if (compressionLevel > LZ4HC_CLEVEL_MAX) compressionLevel = LZ4HC_CLEVEL_MAX;
			LZ4_streamHCPtr->internal_donotuse.compressionLevel = (short)compressionLevel;
		}

		internal static unsafe XXH_errorcode XXH32_reset(XXH32_state* statePtr, uint seed) {
			XXH32_state state;   /* using a local state to Unsafe.CopyBlock() in order to avoid strict-aliasing warnings */
			Unsafe.InitBlock(&state, 0, (uint)sizeof(XXH32_state));
			state.v1 = seed + PRIME32_1 + PRIME32_2;
			state.v2 = seed + PRIME32_2;
			state.v3 = seed + 0;
			state.v4 = seed - PRIME32_1;
			/* do not write into reserved, planned to be removed in a future version */
			Unsafe.CopyBlock(statePtr, &state, (uint)sizeof(XXH32_state) - sizeof(uint));
			return XXH_errorcode.XXH_OK;
		}

		internal static unsafe uint XXH32_digest(XXH32_state* state_in) {

			XXH_endianess endian_detected = BitConverter.IsLittleEndian ? XXH_endianess.XXH_littleEndian : XXH_endianess.XXH_bigEndian;

			if ((endian_detected == XXH_endianess.XXH_littleEndian))
				return XXH32_digest_endian(state_in, XXH_endianess.XXH_littleEndian);
			else
				return XXH32_digest_endian(state_in, XXH_endianess.XXH_bigEndian);
		}

		private static unsafe uint XXH32_digest_endian(XXH32_state* state, XXH_endianess endian) {
			uint h32;

			if (state->large_len != 0) {
				h32 = XXH_rotl32(state->v1, 1)
						+ XXH_rotl32(state->v2, 7)
						+ XXH_rotl32(state->v3, 12)
						+ XXH_rotl32(state->v4, 18);
			}
			else {
				h32 = state->v3 /* == seed */ + PRIME32_5;
			}

			h32 += state->total_len_32;

			return XXH32_finalize(h32, state->mem32, state->memsize, endian, XXH_alignment.XXH_aligned);
		}

		private static uint XXH_rotl32(uint x, int r) {
			return ((x << r) | (x >> (32 - r)));
		}

		private static unsafe uint XXH32_finalize(uint h32, void* ptr, size_t len,
								XXH_endianess endian, XXH_alignment align) {
			byte* p = (byte*)ptr;

			void PROCESS1() {
				h32 += (*p++) * PRIME32_5;
				h32 = XXH_rotl32(h32, 11) * PRIME32_1;
			}

			void PROCESS4() {
				h32 += XXH_readLE32_align(p, endian, align) * PRIME32_3;
				p += 4;
				h32 = XXH_rotl32(h32, 17) * PRIME32_4;
			}

			switch (len & 15)  /* or switch(bEnd - p) */
			{
				case 12:
					PROCESS4();
					PROCESS4();
					PROCESS4();
					return XXH32_avalanche(h32);
				case 8:
					PROCESS4();
					PROCESS4();
					return XXH32_avalanche(h32);
				case 4:
					PROCESS4();
					return XXH32_avalanche(h32);

				case 13:
					PROCESS4();
					PROCESS4();
					PROCESS4();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 9:
					PROCESS4();
					PROCESS4();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 5:
					PROCESS4();
					PROCESS1();
					return XXH32_avalanche(h32);

				case 14:
					PROCESS4();
					PROCESS4();
					PROCESS4();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 10:
					PROCESS4();
					PROCESS4();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 6:
					PROCESS4();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);

				case 15:
					PROCESS4();
					PROCESS4();
					PROCESS4();
					PROCESS1();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 11:
					PROCESS4();
					PROCESS4();
					PROCESS1();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 7:
					PROCESS4();
					PROCESS1();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 3:
					PROCESS1();
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 2:
					PROCESS1();
					PROCESS1();
					return XXH32_avalanche(h32);
				case 1:
					PROCESS1();
					return XXH32_avalanche(h32);
				case 0:
					return XXH32_avalanche(h32);
			}
			Debug.Fail("XXH32_finalize");
			return h32;   /* reaching this point is deemed impossible */
		}

		private static unsafe uint XXH_readLE32_align(void* ptr, XXH_endianess endian, XXH_alignment align) {
			if (align == XXH_alignment.XXH_unaligned)
				return endian == XXH_endianess.XXH_littleEndian ? XXH_read32(ptr) : XXH_swap32(XXH_read32(ptr));
			else
				return endian == XXH_endianess.XXH_littleEndian ? *(uint*)ptr : XXH_swap32(*(uint*)ptr);
		}

		private static unsafe uint XXH_read32(void* memPtr) {
			uint val;
			Unsafe.CopyBlock(&val, memPtr, 4);
			return val;
		}

		private static uint XXH_swap32(uint x) {
			return ((x << 24) & 0xff000000) |
							((x << 8) & 0x00ff0000) |
							((x >> 8) & 0x0000ff00) |
							((x >> 24) & 0x000000ff);
		}

		private static uint XXH32_avalanche(uint h32) {
			h32 ^= h32 >> 15;
			h32 *= PRIME32_2;
			h32 ^= h32 >> 13;
			h32 *= PRIME32_3;
			h32 ^= h32 >> 16;
			return (h32);
		}

		internal static unsafe uint XXH32(void* input, size_t len, uint seed) {
			XXH_endianess endian_detected = BitConverter.IsLittleEndian ? XXH_endianess.XXH_littleEndian : XXH_endianess.XXH_bigEndian;

			if ((endian_detected == XXH_endianess.XXH_littleEndian))
				return XXH32_endian_align(input, len, seed, XXH_endianess.XXH_littleEndian, XXH_alignment.XXH_unaligned);
			else
				return XXH32_endian_align(input, len, seed, XXH_endianess.XXH_bigEndian, XXH_alignment.XXH_unaligned);
		}

		private static unsafe uint XXH32_endian_align(void* input, size_t len, uint seed, XXH_endianess endian, XXH_alignment align) {

			byte* p = (byte*)input;
			byte* bEnd = p + len;
			uint h32;

			if (len >= 16) {
				byte* limit = bEnd - 15;
				uint v1 = seed + PRIME32_1 + PRIME32_2;
				uint v2 = seed + PRIME32_2;
				uint v3 = seed + 0;
				uint v4 = seed - PRIME32_1;

				do {
					v1 = XXH32_round(v1, XXH_readLE32_align(p, endian, align)); p += 4;
					v2 = XXH32_round(v2, XXH_readLE32_align(p, endian, align)); p += 4;
					v3 = XXH32_round(v3, XXH_readLE32_align(p, endian, align)); p += 4;
					v4 = XXH32_round(v4, XXH_readLE32_align(p, endian, align)); p += 4;
				} while (p < limit);

				h32 = XXH_rotl32(v1, 1) + XXH_rotl32(v2, 7)
						+ XXH_rotl32(v3, 12) + XXH_rotl32(v4, 18);
			}
			else {
				h32 = seed + PRIME32_5;
			}

			h32 += (uint)len;

			return XXH32_finalize(h32, p, len & 15, endian, align);
		}

		private static uint XXH32_round(uint seed, uint input) {
			seed += input * PRIME32_2;
			seed = XXH_rotl32(seed, 13);
			seed *= PRIME32_1;
			return seed;
		}

		internal static unsafe XXH_errorcode XXH32_update(XXH32_state* state_in, void* input, size_t len) {
			XXH_endianess endian_detected = BitConverter.IsLittleEndian ? XXH_endianess.XXH_littleEndian : XXH_endianess.XXH_bigEndian;

			if ((endian_detected == XXH_endianess.XXH_littleEndian))
				return XXH32_update_endian(state_in, input, len, XXH_endianess.XXH_littleEndian);
			else
				return XXH32_update_endian(state_in, input, len, XXH_endianess.XXH_bigEndian);
		}

		private static unsafe XXH_errorcode XXH32_update_endian(XXH32_state* state, void* input, size_t len, XXH_endianess endian) {
			if (input == null)
				return XXH_errorcode.XXH_ERROR;

			{
				byte* p = (byte*)input;
				byte* bEnd = p + len;

				state->total_len_32 += (uint)len;
				state->large_len |= ((len >= 16) ? 1u : 0) | ((state->total_len_32 >= 16) ? 1u : 0);

				if (state->memsize + len < 16) {   /* fill in tmp buffer */
					Unsafe.CopyBlock((byte*)(state->mem32) + state->memsize, input, len);
					state->memsize += (uint)len;
					return XXH_errorcode.XXH_OK;
				}

				if (state->memsize != 0) {   /* some data left from previous update */
					Unsafe.CopyBlock((byte*)(state->mem32) + state->memsize, input, 16 - state->memsize);
					{
						uint* p32 = state->mem32;
						state->v1 = XXH32_round(state->v1, XXH_readLE32(p32, endian)); p32++;
						state->v2 = XXH32_round(state->v2, XXH_readLE32(p32, endian)); p32++;
						state->v3 = XXH32_round(state->v3, XXH_readLE32(p32, endian)); p32++;
						state->v4 = XXH32_round(state->v4, XXH_readLE32(p32, endian));
					}
					p += 16 - state->memsize;
					state->memsize = 0;
				}

				if (p <= bEnd - 16) {
					byte* limit = bEnd - 16;
					uint v1 = state->v1;
					uint v2 = state->v2;
					uint v3 = state->v3;
					uint v4 = state->v4;

					do {
						v1 = XXH32_round(v1, XXH_readLE32(p, endian)); p += 4;
						v2 = XXH32_round(v2, XXH_readLE32(p, endian)); p += 4;
						v3 = XXH32_round(v3, XXH_readLE32(p, endian)); p += 4;
						v4 = XXH32_round(v4, XXH_readLE32(p, endian)); p += 4;
					} while (p <= limit);

					state->v1 = v1;
					state->v2 = v2;
					state->v3 = v3;
					state->v4 = v4;
				}

				if (p < bEnd) {
					Unsafe.CopyBlock(state->mem32, p, (size_t)(bEnd - p));
					state->memsize = (uint)(bEnd - p);
				}
			}

			return XXH_errorcode.XXH_OK;
		}

		private static unsafe uint XXH_readLE32(void* ptr, XXH_endianess endian) {
			return XXH_readLE32_align(ptr, endian, XXH_alignment.XXH_unaligned);
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_loadDict(LZ4_stream* LZ4_dict, byte* dictionary, int dictSize) {

			LZ4_stream_t_internal* dict = &LZ4_dict->internal_donotuse;
			tableType_t tableType = tableType_t.byU32;
			byte* p = (byte*)dictionary;
			byte* dictEnd = p + dictSize;
			byte* baseAddr;

			//DEBUGLOG(4, "LZ4_loadDict (%i bytes from %p into %p)", dictSize, dictionary, LZ4_dict);

			/* It's necessary to reset the context,
			 * and not just continue it with prepareTable()
			 * to avoid any risk of generating overflowing matchIndex
			 * when compressing using this dictionary */
			LZ4_resetStream(LZ4_dict);

			/* We always increment the offset by 64 *(1 <<10), since, if the dict is longer,
			 * we truncate it to the last 64k, and if it's shorter, we still want to
			 * advance by a whole window length so we can provide the guarantee that
			 * there are only valid offsets in the window, which allows an optimization
			 * in LZ4_compress_fast_continue() where it uses noDictIssue even when the
			 * dictionary isn't a full 64k. */
			dict->currentOffset += 64 * (1 << 10);

			if (dictSize < (int)HASH_UNIT) {
				return 0;
			}

			if ((dictEnd - p) > 64 * (1 << 10)) p = dictEnd - 64 * (1 << 10);
			baseAddr = dictEnd - dict->currentOffset;
			dict->dictionary = p;
			dict->dictSize = (uint)(dictEnd - p);
			dict->tableType = (ushort)tableType;

			while (p <= dictEnd - HASH_UNIT) {
				LZ4_putPosition(p, dict->hashTable, tableType, baseAddr);
				p += 3;
			}

			return (int)dict->dictSize;
		}

		private static unsafe void LZ4_resetStream(LZ4_stream* LZ4_stream) {
			//DEBUGLOG(5, "LZ4_resetStream (ctx:%p)", LZ4_stream);
			Unsafe.InitBlock(LZ4_stream, 0, (uint)sizeof(LZ4_stream));
		}

		private static unsafe void LZ4_putPosition(byte* p, void* tableBase, tableType_t tableType, byte* srcBase) {
			uint h = LZ4_hashPosition(p, tableType);
			LZ4_putPositionOnHash(p, h, tableBase, tableType, srcBase);
		}

		private static unsafe uint LZ4_hashPosition(void* p, tableType_t tableType) {
			// NOTE: translation: changed sizeof(reg_t)==8 to IntPtr.Size == 8
			if ((IntPtr.Size == 8) && (tableType != tableType_t.byU16)) return LZ4_hash5(LZ4_read_ARCH64(p), tableType);
			return LZ4_hash4(LZ4_read32(p), tableType);
		}

		private static unsafe ulong LZ4_read_ARCH64(void* memPtr) {
			ulong val; Unsafe.CopyBlock(&val, memPtr, 8); return val;
		}

		private static unsafe uint LZ4_read_ARCH32(void* memPtr) {
			uint val; Unsafe.CopyBlock(&val, memPtr, 4); return val;
		}

		private static unsafe uint LZ4_read32(void* memPtr) {
			uint val; Unsafe.CopyBlock(&val, memPtr, 4); return val;
		}

		private static unsafe uint LZ4_hash5(ulong sequence, tableType_t tableType) {

			// NOTE: translation: changed uint to int
			int hashLog = (tableType == tableType_t.byU16) ? LZ4_HASHLOG + 1 : LZ4_HASHLOG;
			if (BitConverter.IsLittleEndian) {
				ulong prime5bytes = 889523592379UL;
				return (uint)(((sequence << 24) * prime5bytes) >> (64 - hashLog));
			}
			else {
				ulong prime8bytes = 11400714785074694791UL;
				return (uint)(((sequence >> 24) * prime8bytes) >> (64 - hashLog));
			}
		}

		private static unsafe uint LZ4_hash4(uint sequence, tableType_t tableType) {
			if (tableType == tableType_t.byU16)
				return ((sequence * 2654435761U) >> ((4 * 8) - (LZ4_HASHLOG + 1)));
			else
				return ((sequence * 2654435761U) >> ((4 * 8) - LZ4_HASHLOG));
		}

		private static unsafe void LZ4_putPositionOnHash(byte* p, uint h,
																		void* tableBase, tableType_t tableType,
															 byte* srcBase) {
			switch (tableType) {
				case tableType_t.clearedTable: { /* illegal! */ Debug.Fail("clearedTable"); return; }
				case tableType_t.byPtr: { byte** hashTable = (byte**)tableBase; hashTable[h] = p; return; }
				case tableType_t.byU32: { uint* hashTable = (uint*)tableBase; hashTable[h] = (uint)(p - srcBase); return; }
				case tableType_t.byU16: { ushort* hashTable = (ushort*)tableBase; hashTable[h] = (ushort)(p - srcBase); return; }
			}
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_loadDictHC(LZ4_streamHC* LZ4_streamHCPtr,
							 byte* dictionary, int dictSize) {

			LZ4HC_CCtx_internal* ctxPtr = &LZ4_streamHCPtr->internal_donotuse;
			//DEBUGLOG(4, "LZ4_loadDictHC(%p, %p, %d)", LZ4_streamHCPtr, dictionary, dictSize);
			Debug.Assert(LZ4_streamHCPtr != null);
			if (dictSize > 64 * (1 << 10)) {
				dictionary += (size_t)dictSize - 64 * (1 << 10);
				dictSize = 64 * (1 << 10);
			}
			/* need a full initialization, there are bad side-effects when using resetFast() */
			{
				int cLevel = ctxPtr->compressionLevel;
				LZ4_initStreamHC(LZ4_streamHCPtr, (uint)sizeof(LZ4_streamHC));
				LZ4_setCompressionLevel(LZ4_streamHCPtr, cLevel);
			}
			LZ4HC_init_internal(ctxPtr, (byte*)dictionary);
			ctxPtr->end = (byte*)dictionary + dictSize;
			if (dictSize >= 4) LZ4HC_Insert(ctxPtr, ctxPtr->end - 3);
			return dictSize;
		}

		private static unsafe void LZ4HC_init_internal(LZ4HC_CCtx_internal* hc4, byte* start) {

			// NOTE: translation: changed uptrval to if
			if (IntPtr.Size == 8) {
				ulong startingOffset = (ulong)(hc4->end - hc4->baseAddr);
				if (startingOffset > 1 * (1U << 30)) {
					LZ4HC_clearTables(hc4);
					startingOffset = 0;
				}
				startingOffset += 64 * (1 << 10);
				hc4->nextToUpdate = (uint)startingOffset;
				hc4->baseAddr = start - startingOffset;
				hc4->end = start;
				hc4->dictBase = start - startingOffset;
				hc4->dictLimit = (uint)startingOffset;
				hc4->lowLimit = (uint)startingOffset;
			}
			else if (IntPtr.Size == 4) {
				uint startingOffset = (uint)(hc4->end - hc4->baseAddr);
				if (startingOffset > 1 * (1U << 30)) {
					LZ4HC_clearTables(hc4);
					startingOffset = 0;
				}
				startingOffset += 64 * (1 << 10);
				hc4->nextToUpdate = (uint)startingOffset;
				hc4->baseAddr = start - startingOffset;
				hc4->end = start;
				hc4->dictBase = start - startingOffset;
				hc4->dictLimit = (uint)startingOffset;
				hc4->lowLimit = (uint)startingOffset;
			}
			else {
				Debug.Fail("IntPtr.Size == " + IntPtr.Size);
			}
		}

		private static unsafe void LZ4HC_clearTables(LZ4HC_CCtx_internal* hc4) {
			// NOTE: translation: changed sizeof(hc4->hashTable) to 4u * lz4.LZ4HC_HASHTABLESIZE
			// NOTE: translation: changed sizeof(hc4->chainTable) to 2u * lz4.LZ4HC_MAXD
			Unsafe.InitBlock((void*)hc4->hashTable, 0, 4u * lz4.LZ4HC_HASHTABLESIZE);
			Unsafe.InitBlock(hc4->chainTable, 0xFF, 2u * lz4.LZ4HC_MAXD);
		}

		private static unsafe void LZ4HC_Insert(LZ4HC_CCtx_internal* hc4, byte* ip) {
			ushort* chainTable = hc4->chainTable;
			uint* hashTable = hc4->hashTable;
			byte* baseAddr = hc4->baseAddr;
			uint target = (uint)(ip - baseAddr);
			uint idx = hc4->nextToUpdate;

			while (idx < target) {
				uint h = LZ4HC_hashPtr(baseAddr + idx);
				size_t delta = idx - hashTable[h];
				if (delta > LZ4_DISTANCE_MAX) delta = LZ4_DISTANCE_MAX;
				chainTable[(ushort)(idx)] = (ushort)delta;
				hashTable[h] = idx;
				idx++;
			}

			hc4->nextToUpdate = target;
		}

		private static unsafe uint LZ4HC_hashPtr(void* ptr) {
			return (((LZ4_read32(ptr)) * 2654435761U) >> ((4 * 8) - LZ4HC_HASH_LOG));
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_compress_fast_continue(LZ4_stream* LZ4_stream,
																 byte* source, byte* dest,
																int inputSize, int maxOutputSize,
																int acceleration) {

			tableType_t tableType = tableType_t.byU32;
			LZ4_stream_t_internal* streamPtr = &LZ4_stream->internal_donotuse;
			byte* dictEnd = streamPtr->dictionary + streamPtr->dictSize;

			//DEBUGLOG(5, "LZ4_compress_fast_continue (inputSize=%i)", inputSize);

			if (streamPtr->dirty != 0) { return 0; } /* Uninitialized structure detected */
			LZ4_renormDictT(streamPtr, inputSize);   /* avoid index overflow */
			if (acceleration < 1) acceleration = ACCELERATION_DEFAULT;

			/* invalidate tiny dictionaries */
			if ((streamPtr->dictSize - 1 < 4 - 1)   /* intentional underflow */
				&& (dictEnd != (byte*)source)) {
				//DEBUGLOG(5, "LZ4_compress_fast_continue: dictSize(%u) at addr:%p is too small", streamPtr->dictSize, streamPtr->dictionary);
				streamPtr->dictSize = 0;
				streamPtr->dictionary = (byte*)source;
				dictEnd = (byte*)source;
			}

			/* Check overlapping input/dictionary space */
			{
				byte* sourceEnd = (byte*)source + inputSize;
				if ((sourceEnd > streamPtr->dictionary) && (sourceEnd < dictEnd)) {
					streamPtr->dictSize = (uint)(dictEnd - sourceEnd);
					if (streamPtr->dictSize > 64 * (1 << 10)) streamPtr->dictSize = 64 * (1 << 10);
					if (streamPtr->dictSize < 4) streamPtr->dictSize = 0;
					streamPtr->dictionary = dictEnd - streamPtr->dictSize;
				}
			}

			/* prefix mode : source data follows dictionary */
			if (dictEnd == (byte*)source) {
				if ((streamPtr->dictSize < 64 * (1 << 10)) && (streamPtr->dictSize < streamPtr->currentOffset))
					return LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.withPrefix64k, dictIssue_directive.dictSmall, acceleration);

				else
					return LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.withPrefix64k, dictIssue_directive.noDictIssue, acceleration);
			}

			/* external dictionary mode */
			{
				int result;
				if (streamPtr->dictCtx != null) {
					/* We depend here on the fact that dictCtx'es (produced by
					 * LZ4_loadDict) guarantee that their tables contain no references
					 * to offsets between dictCtx->currentOffset - 64 *(1 <<10) and
					 * dictCtx->currentOffset - dictCtx->dictSize. This makes it safe
					 * to use noDictIssue even when the dict isn't a full 64 *(1 <<10).
					 */
					if (inputSize > 4 * (1 << 10)) {
						/* For compressing large blobs, it is faster to pay the setup
						 * cost to copy the dictionary's tables into the active context,
						 * so that the compression loop is only looking into one table.
						 */
						Unsafe.CopyBlock(streamPtr, streamPtr->dictCtx, (uint)sizeof(LZ4_stream));
						result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingExtDict, dictIssue_directive.noDictIssue, acceleration);
					}
					else {
						result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingDictCtx, dictIssue_directive.noDictIssue, acceleration);
					}
				}
				else {
					if ((streamPtr->dictSize < 64 * (1 << 10)) && (streamPtr->dictSize < streamPtr->currentOffset)) {
						result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingExtDict, dictIssue_directive.dictSmall, acceleration);
					}
					else {
						result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingExtDict, dictIssue_directive.noDictIssue, acceleration);
					}
				}
				streamPtr->dictionary = (byte*)source;
				streamPtr->dictSize = (uint)inputSize;
				return result;
			}
		}

		private static unsafe void LZ4_renormDictT(LZ4_stream_t_internal* LZ4_dict, int nextSize) {
			Debug.Assert(nextSize >= 0);
			if (LZ4_dict->currentOffset + (uint)nextSize > 0x80000000) {   /* potential ptrdiff_t overflow (32-bits mode) */
																																		 /* rescale hash table */
				uint delta = LZ4_dict->currentOffset - 64 * (1 << 10);
				byte* dictEnd = LZ4_dict->dictionary + LZ4_dict->dictSize;
				int i;
				//DEBUGLOG(4, "LZ4_renormDictT");
				for (i = 0; i < LZ4_HASH_SIZE_U32; i++) {
					if (LZ4_dict->hashTable[i] < delta) LZ4_dict->hashTable[i] = 0;
					else LZ4_dict->hashTable[i] -= delta;
				}
				LZ4_dict->currentOffset = 64 * (1 << 10);
				if (LZ4_dict->dictSize > 64 * (1 << 10)) LZ4_dict->dictSize = 64 * (1 << 10);
				LZ4_dict->dictionary = dictEnd - LZ4_dict->dictSize;
			}
		}

		private static unsafe byte* LZ4_getPositionOnHash(uint h, void* tableBase, tableType_t tableType, byte* srcBase) {
			if (tableType == tableType_t.byPtr) { byte** hashTable = (byte**)tableBase; return hashTable[h]; }
			if (tableType == tableType_t.byU32) { uint* hashTable = (uint*)tableBase; return hashTable[h] + srcBase; }
			{ ushort* hashTable = (ushort*)tableBase; return hashTable[h] + srcBase; }   /* default, to ensure a return */
		}

		private static unsafe byte* LZ4_getPosition(byte* p,
								 void* tableBase, tableType_t tableType,
								 byte* srcBase) {

			uint h = LZ4_hashPosition(p, tableType);
			return LZ4_getPositionOnHash(h, tableBase, tableType, srcBase);
		}

		private static unsafe uint LZ4_getIndexOnHash(uint h, void* tableBase, tableType_t tableType) {
			//LZ4_STATIC_ASSERT(LZ4_MEMORY_USAGE > 2);
			if (tableType == tableType_t.byU32) {
				uint* hashTable = (uint*)tableBase;
				Debug.Assert(h < (1U << (LZ4_MEMORY_USAGE - 2)));
				return hashTable[h];
			}
			if (tableType == tableType_t.byU16) {
				ushort* hashTable = (ushort*)tableBase;
				Debug.Assert(h < (1U << (LZ4_MEMORY_USAGE - 1)));
				return hashTable[h];
			}
			Debug.Fail(tableType.ToString()); return 0;  /* forbidden case */
		}

		private static unsafe void LZ4_putIndexOnHash(uint idx, uint h, void* tableBase, tableType_t tableType) {
			switch (tableType) {
				default: /* fallthrough */
				case tableType_t.clearedTable: /* fallthrough */
				case tableType_t.byPtr: { /* illegal! */ Debug.Fail(tableType.ToString()); return; }
				case tableType_t.byU32: { uint* hashTable = (uint*)tableBase; hashTable[h] = idx; return; }
				case tableType_t.byU16: { ushort* hashTable = (ushort*)tableBase; Debug.Assert(idx < 65536); hashTable[h] = (ushort)idx; return; }
			}
		}

		private static unsafe void LZ4_write32(void* memPtr, uint value) {
			Unsafe.CopyBlock(memPtr, &value, 4);
		}

		private static unsafe void LZ4_writeLE16(void* memPtr, ushort value) {
			if (BitConverter.IsLittleEndian) {
				LZ4_write16(memPtr, value);
			}
			else {
				byte* p = (byte*)memPtr;
				p[0] = (byte)value;
				p[1] = (byte)(value >> 8);
			}
		}

		private static unsafe void LZ4_write16(void* memPtr, ushort value) {
			Unsafe.CopyBlock(memPtr, &value, 2);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compress_generic(
								 LZ4_stream_t_internal* cctx,
									byte* source,
								 byte* dest,
									int inputSize,
								 int* inputConsumed, /* only written when outputDirective == fillOutput */
									int maxOutputSize,
									limitedOutput_directive outputDirective,
									tableType_t tableType,
									dict_directive dictDirective,
									dictIssue_directive dictIssue,
									int acceleration) {

			int result;
			byte* ip = (byte*)source;

			uint startIndex = cctx->currentOffset;
			byte* baseAddr = (byte*)source - startIndex;
			byte* lowLimit;

			LZ4_stream_t_internal* dictCtx = (LZ4_stream_t_internal*)cctx->dictCtx;
			byte* dictionary =
				 dictDirective == dict_directive.usingDictCtx ? dictCtx->dictionary : cctx->dictionary;
			uint dictSize =
				 dictDirective == dict_directive.usingDictCtx ? dictCtx->dictSize : cctx->dictSize;
			uint dictDelta = (dictDirective == dict_directive.usingDictCtx) ? startIndex - dictCtx->currentOffset : 0;   /* make indexes in dictCtx comparable with index in current context */

			// NOTE: translation: changed int to bool
			bool maybe_extMem = (dictDirective == dict_directive.usingExtDict) || (dictDirective == dict_directive.usingDictCtx);
			uint prefixIdxLimit = startIndex - dictSize;   /* used when dictDirective == dictSmall */
			byte* dictEnd = dictionary + dictSize;
			byte* anchor = (byte*)source;
			byte* iend = ip + inputSize;
			byte* mflimitPlusOne = iend - MFLIMIT + 1;
			byte* matchlimit = iend - LASTLITERALS;

			/* the dictCtx currentOffset is indexed on the start of the dictionary,
			 * while a dictionary in the current context precedes the currentOffset */
			byte* dictBase = (dictDirective == dict_directive.usingDictCtx) ?
														 dictionary + dictSize - dictCtx->currentOffset :
														 dictionary + dictSize - startIndex;

			byte* op = (byte*)dest;
			byte* olimit = op + maxOutputSize;

			uint offset = 0;
			uint forwardH;

			//DEBUGLOG(5, "LZ4_compress_generic: srcSize=%i, tableType=%u", inputSize, tableType);
			/* If init conditions are not met, we don't have to mark stream
			 * as having dirty context, since no action was taken yet */
			if (outputDirective == limitedOutput_directive.fillOutput && maxOutputSize < 1) { return 0; } /* Impossible to store anything */
			if ((uint)inputSize > (uint)LZ4_MAX_INPUT_SIZE) { return 0; }           /* Unsupported inputSize, too large (or negative) */
			if ((tableType == tableType_t.byU16) && (inputSize >= LZ4_64Klimit)) { return 0; }  /* Size too large (not within 64K limit) */
			if (tableType == tableType_t.byPtr) Debug.Assert(dictDirective == dict_directive.noDict);      /* only supported use case with byPtr */
			Debug.Assert(acceleration >= 1);

			lowLimit = (byte*)source - (dictDirective == dict_directive.withPrefix64k ? dictSize : 0);

			/* Update context state */
			if (dictDirective == dict_directive.usingDictCtx) {
				/* Subsequent linked blocks can't use the dictionary. */
				/* Instead, they use the block we just compressed. */
				cctx->dictCtx = null;
				cctx->dictSize = (uint)inputSize;
			}
			else {
				cctx->dictSize += (uint)inputSize;
			}
			cctx->currentOffset += (uint)inputSize;
			cctx->tableType = (ushort)tableType;

			if (inputSize < LZ4_minLength) goto _last_literals;        /* Input too small, no compression (all literals) */

			/* First byte */
			LZ4_putPosition(ip, cctx->hashTable, tableType, baseAddr);
			ip++; forwardH = LZ4_hashPosition(ip, tableType);

			/* Main Loop */
			for (; ; ) {
				byte* match;
				byte* token;

				byte* filledIp;

				/* Find a match */
				if (tableType == tableType_t.byPtr) {
					byte* forwardIp = ip;
					int step = 1;
					int searchMatchNb = acceleration << LZ4_skipTrigger;
					do {
						uint h = forwardH;
						ip = forwardIp;
						forwardIp += step;
						step = (searchMatchNb++ >> LZ4_skipTrigger);

						if ((forwardIp > mflimitPlusOne)) goto _last_literals;
						Debug.Assert(ip < mflimitPlusOne);

						match = LZ4_getPositionOnHash(h, cctx->hashTable, tableType, baseAddr);
						forwardH = LZ4_hashPosition(forwardIp, tableType);
						LZ4_putPositionOnHash(ip, h, cctx->hashTable, tableType, baseAddr);

					} while ((match + LZ4_DISTANCE_MAX < ip)
								 || (LZ4_read32(match) != LZ4_read32(ip)));

				}
				else {   /* byuint, byushort */

					byte* forwardIp = ip;
					int step = 1;
					int searchMatchNb = acceleration << LZ4_skipTrigger;
					do {
						uint h = forwardH;
						uint current = (uint)(forwardIp - baseAddr);
						uint matchIndex = LZ4_getIndexOnHash(h, cctx->hashTable, tableType);
						Debug.Assert(matchIndex <= current);
						// NOTE: translation: changed ptrdiff_t
						if (IntPtr.Size == 8)
							Debug.Assert(forwardIp - baseAddr < (long)(2 * (1U << 30) - 1));
						else if (IntPtr.Size == 4)
							Debug.Assert(forwardIp - baseAddr < (int)(2 * (1U << 30) - 1));
						else
							Debug.Fail("IntPtr.Size == " + IntPtr.Size);
						ip = forwardIp;
						forwardIp += step;
						step = (searchMatchNb++ >> LZ4_skipTrigger);

						if ((forwardIp > mflimitPlusOne)) goto _last_literals;
						Debug.Assert(ip < mflimitPlusOne);

						if (dictDirective == dict_directive.usingDictCtx) {
							if (matchIndex < startIndex) {
								/* there was no match, try the dictionary */
								Debug.Assert(tableType == tableType_t.byU32);
								matchIndex = LZ4_getIndexOnHash(h, dictCtx->hashTable, tableType_t.byU32);
								match = dictBase + matchIndex;
								matchIndex += dictDelta;   /* make dictCtx index comparable with current context */
								lowLimit = dictionary;
							}
							else {
								match = baseAddr + matchIndex;
								lowLimit = (byte*)source;
							}
						}
						else if (dictDirective == dict_directive.usingExtDict) {
							if (matchIndex < startIndex) {
								//DEBUGLOG(7, "extDict candidate: matchIndex=%5u  <  startIndex=%5u", matchIndex, startIndex);
								Debug.Assert(startIndex - matchIndex >= 4);
								match = dictBase + matchIndex;
								lowLimit = dictionary;
							}
							else {
								match = baseAddr + matchIndex;
								lowLimit = (byte*)source;
							}
						}
						else {   /* single continuous memory segment */
							match = baseAddr + matchIndex;
						}
						forwardH = LZ4_hashPosition(forwardIp, tableType);
						LZ4_putIndexOnHash(current, h, cctx->hashTable, tableType);

						//DEBUGLOG(7, "candidate at pos=%u  (offset=%u \n", matchIndex, current - matchIndex);
						if ((dictIssue == dictIssue_directive.dictSmall) && (matchIndex < prefixIdxLimit)) { continue; }    /* match outside of valid area */
						Debug.Assert(matchIndex < current);
						if (((tableType != tableType_t.byU16) || (LZ4_DISTANCE_MAX < LZ4_DISTANCE_ABSOLUTE_MAX))
							&& (matchIndex + LZ4_DISTANCE_MAX < current)) {
							continue;
						} /* too far */
						Debug.Assert((current - matchIndex) <= LZ4_DISTANCE_MAX);  /* match now expected within distance */

						if (LZ4_read32(match) == LZ4_read32(ip)) {
							if (maybe_extMem) offset = current - matchIndex;
							break;   /* match found */
						}

					} while (true);
				}

				/* Catch up */
				filledIp = ip;
				while (((ip > anchor) & (match > lowLimit)) && ((ip[-1] == match[-1]))) { ip--; match--; }

				/* Encode Literals */
				{
					uint litLength = (uint)(ip - anchor);
					token = op++;
					if ((outputDirective == limitedOutput_directive.limitedOutput) &&  /* Check output buffer overflow */
							((op + litLength + (2 + 1 + LASTLITERALS) + (litLength / 255) > olimit))) {
						return 0;   /* cannot compress within `dst` budget. Stored indexes in hash table are nonetheless fine */
					}
					if ((outputDirective == limitedOutput_directive.fillOutput) &&
							((op + (litLength + 240) / 255 /* litlen */ + litLength /* literals */ + 2 /* offset */ + 1 /* token */ + MFLIMIT - 4 /* min last literals so last match is <= end - MFLIMIT */ > olimit))) {
						op--;
						goto _last_literals;
					}
					if (litLength >= ((1U << (8 - 4)) - 1)) {
						int len = (int)(litLength - ((1U << (8 - 4)) - 1));

						// NOTE: translation: added byte cast
						*token = (byte)(((1U << (8 - 4)) - 1) << 4);
						for (; len >= 255; len -= 255) *op++ = 255;

						*op++ = (byte)len;
					}
					else *token = (byte)(litLength << 4);

					/* Copy Literals */
					LZ4_wildCopy8(op, anchor, op + litLength);
					op += litLength;
					//DEBUGLOG(6, "seq.start:%i, literals=%u, match.start:%i",
					//						(int)(anchor - (byte*)source), litLength, (int)(ip - (byte*)source));
				}

			_next_match:
				/* at this stage, the following variables must be correctly set :
				 * - ip : at start of LZ operation
				 * - match : at start of previous pattern occurence; can be within current prefix, or within extDict
				 * - offset : if maybe_ext_memSegment==1 (ant)
				 * - lowLimit : must be == dictionary to mean "match is within extDict"; must be == source otherwise
				 * - token and *token : position to write 4-bits for match length; higher 4-bits for literal length supposed already written
				 */

				if ((outputDirective == limitedOutput_directive.fillOutput) &&
						(op + 2 /* offset */ + 1 /* token */ + MFLIMIT - 4 /* min last literals so last match is <= end - MFLIMIT */ > olimit)) {
					/* the match was too close to the end, rewind and go to last literals */
					op = token;
					goto _last_literals;
				}

				/* Encode Offset */
				if (maybe_extMem) {   /* static test */

					//DEBUGLOG(6, "             with offset=%u  (ext if > %i)", offset, (int)(ip - (byte*)source));
					Debug.Assert(offset <= LZ4_DISTANCE_MAX && offset > 0);
					LZ4_writeLE16(op, (ushort)offset); op += 2;
				}
				else {
					//DEBUGLOG(6, "             with offset=%u  (same segment)", (uint)(ip - match));
					Debug.Assert(ip - match <= LZ4_DISTANCE_MAX);
					LZ4_writeLE16(op, (ushort)(ip - match)); op += 2;
				}

				/* Encode MatchLength */
				{
					uint matchCode;

					if ((dictDirective == dict_directive.usingExtDict || dictDirective == dict_directive.usingDictCtx)
						&& (lowLimit == dictionary) /* match within extDict */ ) {
						byte* limit = ip + (dictEnd - match);
						Debug.Assert(dictEnd > match);
						if (limit > matchlimit) limit = matchlimit;
						matchCode = LZ4_count(ip + 4, match + 4, limit);
						ip += (size_t)matchCode + 4;
						if (ip == limit) {
							uint more = LZ4_count(limit, (byte*)source, matchlimit);
							matchCode += more;
							ip += more;
						}
						//DEBUGLOG(6, "             with matchLength=%u starting in extDict", matchCode + 4);
					}
					else {
						matchCode = LZ4_count(ip + 4, match + 4, matchlimit);
						ip += (size_t)matchCode + 4;
						//DEBUGLOG(6, "             with matchLength=%u", matchCode + 4);
					}

					if ((outputDirective != limitedOutput_directive.notLimited) &&    /* Check output buffer overflow */
							((op + (1 + LASTLITERALS) + (matchCode + 240) / 255 > olimit))) {
						if (outputDirective == limitedOutput_directive.fillOutput) {
							/* Match description too long : reduce it */
							uint newMatchCode = 15 /* in token */ - 1 /* to avoid needing a zero byte */ + ((uint)(olimit - op) - 1 - LASTLITERALS) * 255;
							ip -= matchCode - newMatchCode;
							Debug.Assert(newMatchCode < matchCode);
							matchCode = newMatchCode;
							if ((ip <= filledIp)) {
								/* We have already filled up to filledIp so if ip ends up less than filledIp
								 * we have positions in the hash table beyond the current position. This is
								 * a problem if we reuse the hash table. So we have to remove these positions
								 * from the hash table.
								 */
								byte* ptr;
								//DEBUGLOG(5, "Clearing %u positions", (uint)(filledIp - ip));
								for (ptr = ip; ptr <= filledIp; ++ptr) {
									uint h = LZ4_hashPosition(ptr, tableType);
									LZ4_clearHash(h, cctx->hashTable, tableType);
								}
							}
						}
						else {
							Debug.Assert(outputDirective == limitedOutput_directive.limitedOutput);
							return 0;   /* cannot compress within `dst` budget. Stored indexes in hash table are nonetheless fine */
						}
					}
					if (matchCode >= ((1U << 4) - 1)) {

						// NOTE: translation: add byte cast
						*token += (byte)((1U << 4) - 1);
						matchCode -= ((1U << 4) - 1);
						LZ4_write32(op, 0xFFFFFFFF);
						while (matchCode >= 4 * 255) {
							op += 4;
							LZ4_write32(op, 0xFFFFFFFF);
							matchCode -= 4 * 255;
						}
						op += matchCode / 255;

						*op++ = (byte)(matchCode % 255);
					}
					else

						*token += (byte)(matchCode);
				}
				/* Ensure we have enough space for the last literals. */
				Debug.Assert(!(outputDirective == limitedOutput_directive.fillOutput && op + 1 + LASTLITERALS > olimit));

				anchor = ip;

				/* Test end of chunk */
				if (ip >= mflimitPlusOne) break;

				/* Fill table */
				LZ4_putPosition(ip - 2, cctx->hashTable, tableType, baseAddr);

				/* Test next position */
				if (tableType == tableType_t.byPtr) {

					match = LZ4_getPosition(ip, cctx->hashTable, tableType, baseAddr);
					LZ4_putPosition(ip, cctx->hashTable, tableType, baseAddr);
					if ((match + LZ4_DISTANCE_MAX >= ip)
						&& (LZ4_read32(match) == LZ4_read32(ip))) { token = op++; *token = 0; goto _next_match; }

				}
				else {   /* byuint, byushort */

					uint h = LZ4_hashPosition(ip, tableType);
					uint current = (uint)(ip - baseAddr);
					uint matchIndex = LZ4_getIndexOnHash(h, cctx->hashTable, tableType);
					Debug.Assert(matchIndex < current);
					if (dictDirective == dict_directive.usingDictCtx) {
						if (matchIndex < startIndex) {
							/* there was no match, try the dictionary */
							matchIndex = LZ4_getIndexOnHash(h, dictCtx->hashTable, tableType_t.byU32);
							match = dictBase + matchIndex;
							lowLimit = dictionary;   /* required for match length counter */
							matchIndex += dictDelta;
						}
						else {
							match = baseAddr + matchIndex;
							lowLimit = (byte*)source;  /* required for match length counter */
						}
					}
					else if (dictDirective == dict_directive.usingExtDict) {
						if (matchIndex < startIndex) {
							match = dictBase + matchIndex;
							lowLimit = dictionary;   /* required for match length counter */
						}
						else {
							match = baseAddr + matchIndex;
							lowLimit = (byte*)source;   /* required for match length counter */
						}
					}
					else {   /* single memory segment */
						match = baseAddr + matchIndex;
					}
					LZ4_putIndexOnHash(current, h, cctx->hashTable, tableType);
					Debug.Assert(matchIndex < current);
					if (((dictIssue == dictIssue_directive.dictSmall) ? (matchIndex >= prefixIdxLimit) : true)
						&& (((tableType == tableType_t.byU16) && (LZ4_DISTANCE_MAX == LZ4_DISTANCE_ABSOLUTE_MAX)) ? true : (matchIndex + LZ4_DISTANCE_MAX >= current))
						&& (LZ4_read32(match) == LZ4_read32(ip))) {
						token = op++;

						*token = 0;
						if (maybe_extMem) offset = current - matchIndex;
						//DEBUGLOG(6, "seq.start:%i, literals=%u, match.start:%i",
						//						(int)(anchor - (byte*)source), 0, (int)(ip - (byte*)source));
						goto _next_match;
					}
				}

				/* Prepare next loop */
				forwardH = LZ4_hashPosition(++ip, tableType);

			}

		_last_literals:
			/* Encode Last Literals */
			{
				size_t lastRun = (size_t)(iend - anchor);
				if ((outputDirective != limitedOutput_directive.notLimited) &&  /* Check output buffer overflow */
						(op + lastRun + 1 + ((lastRun + 255 - ((1U << (8 - 4)) - 1)) / 255) > olimit)) {
					if (outputDirective == limitedOutput_directive.fillOutput) {
						/* adapt lastRun to fill 'dst' */
						Debug.Assert(olimit >= op);
						lastRun = (size_t)(olimit - op) - 1;
						lastRun -= (lastRun + 240) / 255;
					}
					else {
						Debug.Assert(outputDirective == limitedOutput_directive.limitedOutput);
						return 0;   /* cannot compress within `dst` budget. Stored indexes in hash table are nonetheless fine */
					}
				}
				if (lastRun >= ((1U << (8 - 4)) - 1)) {
					size_t accumulator = lastRun - ((1U << (8 - 4)) - 1);

					// NOTE: translation: added byte cast
					*op++ = (byte)(((1U << (8 - 4)) - 1) << 4);
					for (; accumulator >= 255; accumulator -= 255) *op++ = 255;

					*op++ = (byte)accumulator;
				}
				else {

					*op++ = (byte)(lastRun << 4);
				}
				Unsafe.CopyBlock(op, anchor, lastRun);
				ip = anchor + lastRun;
				op += lastRun;
			}

			if (outputDirective == limitedOutput_directive.fillOutput) {
				// NOTE: translation: changed char* to byte*
				*inputConsumed = (int)(((byte*)ip) - source);
			}
			//DEBUGLOG(5, "LZ4_compress_generic: compressed %i bytes into %i bytes", inputSize, (int)(((char*)op) - dest));
			// NOTE: translation: changed char* to byte*
			result = (int)(((byte*)op) - dest);
			Debug.Assert(result > 0);
			return result;
		}

		private static unsafe void LZ4_wildCopy8(void* dstPtr, void* srcPtr, void* dstEnd) {

			byte* d = (byte*)dstPtr;
			byte* s = (byte*)srcPtr;
			byte* e = (byte*)dstEnd;

			do { Unsafe.CopyBlock(d, s, 8); d += 8; s += 8; } while (d < e);
		}

		private static unsafe uint LZ4_count(byte* pIn, byte* pMatch, byte* pInLimit) {

			byte* pStart = pIn;
			// NOTE: translation: changed reg_t to if
			// NOTE: translation: changed STEPSIZE to IntPtr.Size
			int STEPSIZE = IntPtr.Size;
			if (IntPtr.Size == 8) {
				if ((pIn < pInLimit - (STEPSIZE - 1))) {
					ulong diff = LZ4_read_ARCH64(pMatch) ^ LZ4_read_ARCH64(pIn);
					if (diff == 0) {
						pIn += STEPSIZE; pMatch += STEPSIZE;
					}
					else {
						return LZ4_NbCommonBytes64(diff);
					}
				}

				while ((pIn < pInLimit - (STEPSIZE - 1))) {
					ulong diff = LZ4_read_ARCH64(pMatch) ^ LZ4_read_ARCH64(pIn);
					if (diff == 0) { pIn += STEPSIZE; pMatch += STEPSIZE; continue; }
					pIn += LZ4_NbCommonBytes64(diff);
					return (uint)(pIn - pStart);
				}
			}
			else if (IntPtr.Size == 4) {
				if ((pIn < pInLimit - (STEPSIZE - 1))) {
					uint diff = LZ4_read_ARCH32(pMatch) ^ LZ4_read_ARCH32(pIn);
					if (diff == 0) {
						pIn += STEPSIZE; pMatch += STEPSIZE;
					}
					else {
						return LZ4_NbCommonBytes32(diff);
					}
				}

				while ((pIn < pInLimit - (STEPSIZE - 1))) {
					uint diff = LZ4_read_ARCH32(pMatch) ^ LZ4_read_ARCH32(pIn);
					if (diff == 0) { pIn += STEPSIZE; pMatch += STEPSIZE; continue; }
					pIn += LZ4_NbCommonBytes32(diff);
					return (uint)(pIn - pStart);
				}
			}
			else {
				Debug.Fail("IntPtr.Size == " + IntPtr.Size);
			}

			if ((STEPSIZE == 8) && (pIn < (pInLimit - 3)) && (LZ4_read32(pMatch) == LZ4_read32(pIn))) { pIn += 4; pMatch += 4; }
			if ((pIn < (pInLimit - 1)) && (LZ4_read16(pMatch) == LZ4_read16(pIn))) { pIn += 2; pMatch += 2; }
			if ((pIn < pInLimit) && (*pMatch == *pIn)) pIn++;
			return (uint)(pIn - pStart);
		}

		private static unsafe ushort LZ4_read16(void* memPtr) {
			ushort val; Unsafe.CopyBlock(&val, memPtr, 2); return val;
		}

		private static unsafe uint LZ4_NbCommonBytes64(ulong val) {
			if (BitConverter.IsLittleEndian) {
				// NOTE: translation: added ulong cast
				return DeBruijnBytePos64[((ulong)((val & (ulong)-(long)val) * 0x0218A392CDABBD3FUL)) >> 58];
			}
			else   /* Big Endian CPU */ {
				int by32 = 8 * 4;  /* 32 on 64 bits (goal), 16 on 32 bits.
                Just to avoid some static analyzer complaining about shift by 32 on 32-bits target.
                Note that this code path is never triggered in 32-bits mode. */
				uint r;
				if ((val >> by32) == 0) { r = 4; } else { r = 0; val >>= by32; }
				if ((val >> 16) == 0) { r += 2; val >>= 8; } else { val >>= 24; }
				r += (val == 0 ? 1u : 0);
				return r;
			}
		}

		private static unsafe uint LZ4_NbCommonBytes32(uint val) {
			if (BitConverter.IsLittleEndian) {
				return DeBruijnBytePos32[((uint)((val & -(int)val) * 0x077CB531U)) >> 27];
			}
			else   /* Big Endian CPU */ {
				uint r;
				if ((val >> 16) == 0) { r = 2; val >>= 8; } else { r = 0; val >>= 24; }
				r += (val == 0 ? 1u : 0);
				return r;
			}
		}

		private static unsafe void LZ4_clearHash(uint h, void* tableBase, tableType_t tableType) {
			switch (tableType) {
				default: /* fallthrough */
				case tableType_t.clearedTable: { /* illegal! */ Debug.Fail("clearedTable"); return; }
				case tableType_t.byPtr: { byte** hashTable = (byte**)tableBase; hashTable[h] = null; return; }
				case tableType_t.byU32: { uint* hashTable = (uint*)tableBase; hashTable[h] = 0; return; }
				case tableType_t.byU16: { ushort* hashTable = (ushort*)tableBase; hashTable[h] = 0; return; }
			}
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_compress_HC_continue(LZ4_streamHC* LZ4_streamHCPtr, byte* src, byte* dst, int srcSize, int dstCapacity) {
			if (dstCapacity < LZ4_compressBound(srcSize))
				return LZ4_compressHC_continue_generic(LZ4_streamHCPtr, src, dst, &srcSize, dstCapacity, limitedOutput_directive.limitedOutput);
			else
				return LZ4_compressHC_continue_generic(LZ4_streamHCPtr, src, dst, &srcSize, dstCapacity, limitedOutput_directive.notLimited);
		}

		internal static int LZ4_compressBound(int isize) {
			return ((uint)(isize) > (uint)LZ4_MAX_INPUT_SIZE ? 0 : (isize) + ((isize) / 255) + 16);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compressHC_continue_generic(LZ4_streamHC* LZ4_streamHCPtr,
																							 byte* src, byte* dst,
																							int* srcSizePtr, int dstCapacity,
																							limitedOutput_directive limit) {
			LZ4HC_CCtx_internal* ctxPtr = &LZ4_streamHCPtr->internal_donotuse;
			//DEBUGLOG(4, "LZ4_compressHC_continue_generic(ctx=%p, src=%p, srcSize=%d)",
			//						LZ4_streamHCPtr, src, *srcSizePtr);
			Debug.Assert(ctxPtr != null);
			/* auto-init if forgotten */
			if (ctxPtr->baseAddr == null) LZ4HC_init_internal(ctxPtr, (byte*)src);

			/* Check overflow */
			if ((size_t)(ctxPtr->end - ctxPtr->baseAddr) > 2 * (1U << 30)) {
				size_t dictSize = (size_t)(ctxPtr->end - ctxPtr->baseAddr) - ctxPtr->dictLimit;
				if (dictSize > 64 * (1 << 10)) dictSize = 64 * (1 << 10);
				// NOTE: translation: changed char* to byte*
				LZ4_loadDictHC(LZ4_streamHCPtr, (byte*)(ctxPtr->end) - dictSize, (int)dictSize);
			}

			/* Check if blocks follow each other */
			if ((byte*)src != ctxPtr->end)
				LZ4HC_setExternalDict(ctxPtr, (byte*)src);

			/* Check overlapping input/dictionary space */
			{
				byte* sourceEnd = (byte*)src + *srcSizePtr;
				byte* dictBegin = ctxPtr->dictBase + ctxPtr->lowLimit;
				byte* dictEnd = ctxPtr->dictBase + ctxPtr->dictLimit;
				if ((sourceEnd > dictBegin) && ((byte*)src < dictEnd)) {
					if (sourceEnd > dictEnd) sourceEnd = dictEnd;
					ctxPtr->lowLimit = (uint)(sourceEnd - ctxPtr->dictBase);
					if (ctxPtr->dictLimit - ctxPtr->lowLimit < 4) ctxPtr->lowLimit = ctxPtr->dictLimit;
				}
			}

			return LZ4HC_compress_generic(ctxPtr, src, dst, srcSizePtr, dstCapacity, ctxPtr->compressionLevel, limit);
		}

		private static unsafe void LZ4HC_setExternalDict(LZ4HC_CCtx_internal* ctxPtr, byte* newBlock) {
			//DEBUGLOG(4, "LZ4HC_setExternalDict(%p, %p)", ctxPtr, newBlock);
			if (ctxPtr->end >= ctxPtr->baseAddr + ctxPtr->dictLimit + 4)
				LZ4HC_Insert(ctxPtr, ctxPtr->end - 3);   /* Referencing remaining dictionary content */

			/* Only one memory segment for extDict, so any previous extDict is lost at this stage */
			ctxPtr->lowLimit = ctxPtr->dictLimit;
			ctxPtr->dictLimit = (uint)(ctxPtr->end - ctxPtr->baseAddr);
			ctxPtr->dictBase = ctxPtr->baseAddr;
			ctxPtr->baseAddr = newBlock - ctxPtr->dictLimit;
			ctxPtr->end = newBlock;
			ctxPtr->nextToUpdate = ctxPtr->dictLimit;   /* match referencing will resume from there */

			/* cannot reference an extDict and a dictCtx at the same time */
			ctxPtr->dictCtx = null;
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_generic(
					LZ4HC_CCtx_internal* ctx,
					 byte* src,
					byte* dst,
					int* srcSizePtr,
					int dstCapacity,
					int cLevel,
					limitedOutput_directive limit
					) {
			if (ctx->dictCtx == null) {
				return LZ4HC_compress_generic_noDictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
			}
			else {
				return LZ4HC_compress_generic_dictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
			}
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_generic_noDictCtx(
					LZ4HC_CCtx_internal* ctx,
					 byte* src,
					byte* dst,
					int* srcSizePtr,
					int dstCapacity,
					int cLevel,
					limitedOutput_directive limit
					) {
			Debug.Assert(ctx->dictCtx == null);
			return LZ4HC_compress_generic_internal(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit, dictCtx_directive.noDictCtx);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_generic_dictCtx(
					LZ4HC_CCtx_internal* ctx,
					 byte* src,
					byte* dst,
					int* srcSizePtr,
					int dstCapacity,
					int cLevel,
					limitedOutput_directive limit
					) {
			size_t position = (size_t)(ctx->end - ctx->baseAddr) - ctx->lowLimit;
			Debug.Assert(ctx->dictCtx != null);
			if (position >= 64 * (1 << 10)) {
				ctx->dictCtx = null;
				return LZ4HC_compress_generic_noDictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
			}
			else if (position == 0 && *srcSizePtr > 4 * (1 << 10)) {
				Unsafe.CopyBlock(ctx, ctx->dictCtx, (uint)sizeof(LZ4HC_CCtx_internal));
				LZ4HC_setExternalDict(ctx, (byte*)src);
				ctx->compressionLevel = (short)cLevel;
				return LZ4HC_compress_generic_noDictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
			}
			else {
				return LZ4HC_compress_generic_internal(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit, dictCtx_directive.usingDictCtxHc);
			}
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_generic_internal(
			LZ4HC_CCtx_internal* ctx,
			 byte* src,
			byte* dst,
			int* srcSizePtr,
			int dstCapacity,
			int cLevel,
			 limitedOutput_directive limit,
			 dictCtx_directive dict
			) {

			//DEBUGLOG(4, "LZ4HC_compress_generic(ctx=%p, src=%p, srcSize=%d)", ctx, src, *srcSizePtr);

			if (limit == limitedOutput_directive.fillOutput && dstCapacity < 1) return 0;   /* Impossible to store anything */
			if ((uint)*srcSizePtr > (uint)LZ4_MAX_INPUT_SIZE) return 0;    /* Unsupported input size (too large or negative) */

			ctx->end += *srcSizePtr;
			if (cLevel < 1) cLevel = LZ4HC_CLEVEL_DEFAULT;   /* note : convention is different from lz4frame, maybe something to review */
			cLevel = Math.Min(LZ4HC_CLEVEL_MAX, cLevel);
			{
				cParams_t cParam = clTable[cLevel];
				HCfavor_e favor = ctx->favorDecSpeed != 0 ? HCfavor_e.favorDecompressionSpeed : HCfavor_e.favorCompressionRatio;
				int result;

				if (cParam.strat == lz4hc_strat_e.lz4hc) {
					result = LZ4HC_compress_hashChain(ctx,
															src, dst, srcSizePtr, dstCapacity,
															cParam.nbSearches, limit, dict);
				}
				else {
					Debug.Assert(cParam.strat == lz4hc_strat_e.lz4opt);
					result = LZ4HC_compress_optimal(ctx,
															src, dst, srcSizePtr, dstCapacity,
															(int)cParam.nbSearches, cParam.targetLength, limit,
															cLevel == LZ4HC_CLEVEL_MAX,   /* ultra mode */
															dict, favor);
				}
				if (result <= 0) ctx->dirty = 1;
				return result;
			}
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_hashChain(
			LZ4HC_CCtx_internal* ctx,
			 byte* source,
			byte* dest,
			int* srcSizePtr,
			int maxOutputSize,
			uint maxNbAttempts,
			 limitedOutput_directive limit,
			 dictCtx_directive dict
			) {
			int inputSize = *srcSizePtr;
			// NOTE: translation: changed from int to bool
			bool patternAnalysis = (maxNbAttempts > 128);   /* levels 9+ */

			byte* ip = (byte*)source;
			byte* anchor = ip;
			byte* iend = ip + inputSize;
			byte* mflimit = iend - MFLIMIT;
			byte* matchlimit = (iend - LASTLITERALS);

			byte* optr = (byte*)dest;
			byte* op = (byte*)dest;
			byte* oend = op + maxOutputSize;

			int ml0, ml, ml2, ml3;
			byte* start0;
			byte* ref0;
			byte* refAddr = null;
			byte* start2 = null;
			byte* ref2 = null;
			byte* start3 = null;
			byte* ref3 = null;

			/* init */
			*srcSizePtr = 0;
			if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;                  /* Hack for support LZ4 format restriction */
			if (inputSize < LZ4_minLength) goto _last_literals;                  /* Input too small, no compression (all literals) */

			/* Main Loop */
			while (ip <= mflimit) {
				// NOTE: translation: added cast to int
				ml = LZ4HC_InsertAndFindBestMatch(ctx, ip, matchlimit, &refAddr, (int)maxNbAttempts, patternAnalysis, dict);
				if (ml < 4) { ip++; continue; }

				/* saved, in case we would skip too much */
				start0 = ip; ref0 = refAddr; ml0 = ml;

			_Search2:
				if (ip + ml <= mflimit) {
					ml2 = LZ4HC_InsertAndGetWiderMatch(ctx,
													ip + ml - 2, ip + 0, matchlimit, ml, &ref2, &start2,
													// NOTE: translation: added cast to int
													(int)maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
				}
				else {
					ml2 = ml;
				}

				if (ml2 == ml) { /* No better match => encode ML1 */
					optr = op;
					if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, refAddr, limit, oend)) goto _dest_overflow;
					continue;
				}

				if (start0 < ip) {   /* first match was skipped at least once */
					if (start2 < ip + ml0) {  /* squeezing ML1 between ML0(original ML1) and ML2 */
						ip = start0; refAddr = ref0; ml = ml0;  /* restore initial ML1 */
					}
				}

				/* Here, start0==ip */
				if ((start2 - ip) < 3) {  /* First Match too small : removed */
					ml = ml2;
					ip = start2;
					refAddr = ref2;
					goto _Search2;
				}

			_Search3:
				/* At this stage, we have :
				*  ml2 > ml1, and
				*  ip1+3 <= ip2 (usually < ip1+ml1) */
				if ((start2 - ip) < (int)((((1U << 4) - 1) - 1) + 4)) {
					int correction;
					int new_ml = ml;
					if (new_ml > (int)((((1U << 4) - 1) - 1) + 4)) new_ml = (int)((((1U << 4) - 1) - 1) + 4);
					if (ip + new_ml > start2 + ml2 - 4) new_ml = (int)(start2 - ip) + ml2 - 4;
					correction = new_ml - (int)(start2 - ip);
					if (correction > 0) {
						start2 += correction;
						ref2 += correction;
						ml2 -= correction;
					}
				}
				/* Now, we have start2 = ip+new_ml, with new_ml = min(ml, (int)((((1U<<4)-1)-1)+4)=18) */

				if (start2 + ml2 <= mflimit) {
					ml3 = LZ4HC_InsertAndGetWiderMatch(ctx,
													start2 + ml2 - 3, start2, matchlimit, ml2, &ref3, &start3,
													// NOTE: translation: added cast to int
													(int)maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
				}
				else {
					ml3 = ml2;
				}

				if (ml3 == ml2) {  /* No better match => encode ML1 and ML2 */
													 /* ip & ref are known; Now for ml */
					if (start2 < ip + ml) ml = (int)(start2 - ip);
					/* Now, encode 2 sequences */
					optr = op;
					if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, refAddr, limit, oend)) goto _dest_overflow;
					ip = start2;
					optr = op;
					if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml2, ref2, limit, oend)) goto _dest_overflow;
					continue;
				}

				if (start3 < ip + ml + 3) {  /* Not enough space for match 2 : remove it */
					if (start3 >= (ip + ml)) {  /* can write Seq1 immediately ==> Seq2 is removed, so Seq3 becomes Seq1 */
						if (start2 < ip + ml) {
							int correction = (int)(ip + ml - start2);
							start2 += correction;
							ref2 += correction;
							ml2 -= correction;
							if (ml2 < 4) {
								start2 = start3;
								ref2 = ref3;
								ml2 = ml3;
							}
						}

						optr = op;
						if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, refAddr, limit, oend)) goto _dest_overflow;
						ip = start3;
						refAddr = ref3;
						ml = ml3;

						start0 = start2;
						ref0 = ref2;
						ml0 = ml2;
						goto _Search2;
					}

					start2 = start3;
					ref2 = ref3;
					ml2 = ml3;
					goto _Search3;
				}

				/*
				* OK, now we have 3 ascending matches;
				* let's write the first one ML1.
				* ip & ref are known; Now decide ml.
				*/
				if (start2 < ip + ml) {
					if ((start2 - ip) < (int)((((1U << 4) - 1) - 1) + 4)) {
						int correction;
						if (ml > (int)((((1U << 4) - 1) - 1) + 4)) ml = (int)((((1U << 4) - 1) - 1) + 4);
						if (ip + ml > start2 + ml2 - 4) ml = (int)(start2 - ip) + ml2 - 4;
						correction = ml - (int)(start2 - ip);
						if (correction > 0) {
							start2 += correction;
							ref2 += correction;
							ml2 -= correction;
						}
					}
					else {
						ml = (int)(start2 - ip);
					}
				}
				optr = op;
				if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, refAddr, limit, oend)) goto _dest_overflow;

				/* ML2 becomes ML1 */
				ip = start2; refAddr = ref2; ml = ml2;

				/* ML3 becomes ML2 */
				start2 = start3; ref2 = ref3; ml2 = ml3;

				/* let's find a new ML3 */
				goto _Search3;
			}

		_last_literals:
			/* Encode Last Literals */
			{
				size_t lastRunSize = (size_t)(iend - anchor);  /* literals */
				size_t litLength = (lastRunSize + 255 - ((1U << (8 - 4)) - 1)) / 255;
				size_t totalSize = 1 + litLength + lastRunSize;
				if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
				if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
					if (limit == limitedOutput_directive.limitedOutput) return 0;  /* Check output limit */
																																				 /* adapt lastRunSize to fill 'dest' */
					lastRunSize = (size_t)(oend - op) - 1;
					litLength = (lastRunSize + 255 - ((1U << (8 - 4)) - 1)) / 255;
					lastRunSize -= litLength;
				}
				ip = anchor + lastRunSize;

				if (lastRunSize >= ((1U << (8 - 4)) - 1)) {
					size_t accumulator = lastRunSize - ((1U << (8 - 4)) - 1);
					// NOTE: translation: add byte cast
					*op++ = (byte)(((1U << (8 - 4)) - 1) << 4);
					for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
					*op++ = (byte)accumulator;
				}
				else {
					*op++ = (byte)(lastRunSize << 4);
				}
				Unsafe.CopyBlock(op, anchor, lastRunSize);
				op += lastRunSize;
			}

			/* End */
			// NOTE: translation: changed char* to byte*
			*srcSizePtr = (int)(((byte*)ip) - source);
			// NOTE: translation: changed char* to byte*
			return (int)(((byte*)op) - dest);

		_dest_overflow:
			if (limit == limitedOutput_directive.fillOutput) {
				op = optr;  /* restore correct out pointer */
				goto _last_literals;
			}
			return 0;
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4HC_compress_optimal(LZ4HC_CCtx_internal* ctx,
																				 byte* source,
																				byte* dst,
																				int* srcSizePtr,
																				int dstCapacity,
																				int nbSearches,
																				size_t sufficient_len,
																				 limitedOutput_directive limit,
																				// NOTE: translation: changed from int to bool
																				bool fullUpdate,
																				 dictCtx_directive dict,
																				 HCfavor_e favorDecSpeed) {
			// NOTE: translation: changed from stackalloc to heap
			LZ4HC_optimal_t[] opt = new LZ4HC_optimal_t[LZ4_OPT_NUM + TRAILING_LITERALS];   /* ~64 *(1 <<10), which is a bit large for stack... */

			byte* ip = (byte*)source;
			byte* anchor = ip;
			byte* iend = ip + *srcSizePtr;
			byte* mflimit = iend - MFLIMIT;
			byte* matchlimit = iend - LASTLITERALS;
			byte* op = (byte*)dst;
			byte* opSaved = (byte*)dst;
			byte* oend = op + dstCapacity;

			/* init */
			//DEBUGLOG(5, "LZ4HC_compress_optimal(dst=%p, dstCapa=%u)", dst, (uint)dstCapacity);
			*srcSizePtr = 0;
			if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;   /* Hack for support LZ4 format restriction */
			if (sufficient_len >= LZ4_OPT_NUM) sufficient_len = LZ4_OPT_NUM - 1;

			/* Main Loop */
			Debug.Assert(ip - anchor < LZ4_MAX_INPUT_SIZE);
			while (ip <= mflimit) {
				int llen = (int)(ip - anchor);
				int best_mlen, best_off;
				int cur, last_match_pos = 0;

				LZ4HC_match_t firstMatch = LZ4HC_FindLongerMatch(ctx, ip, matchlimit, 4 - 1, nbSearches, dict, favorDecSpeed);
				if (firstMatch.len == 0) { ip++; continue; }

				if ((size_t)firstMatch.len > sufficient_len) {
					/* good enough solution : immediate encoding */
					int firstML = firstMatch.len;
					byte* matchPos = ip - firstMatch.off;
					opSaved = op;
					if (LZ4HC_encodeSequence(&ip, &op, &anchor, firstML, matchPos, limit, oend))   /* updates ip, op and anchor */
						goto _dest_overflow;
					continue;
				}

				/* set prices for first positions (literals) */
				{
					int rPos;
					for (rPos = 0; rPos < 4; rPos++) {
						int cost = LZ4HC_literalsPrice(llen + rPos);
						opt[rPos].mlen = 1;
						opt[rPos].off = 0;
						opt[rPos].litlen = llen + rPos;
						opt[rPos].price = cost;
						//DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i) -- initial setup",
						//						rPos, cost, opt[rPos].litlen);
					}
				}
				/* set prices using initial match */
				{
					int mlen = 4;
					int matchML = firstMatch.len;   /* necessarily < sufficient_len < LZ4_OPT_NUM */
					int offset = firstMatch.off;
					Debug.Assert(matchML < LZ4_OPT_NUM);
					for (; mlen <= matchML; mlen++) {
						int cost = LZ4HC_sequencePrice(llen, mlen);
						opt[mlen].mlen = mlen;
						opt[mlen].off = offset;
						opt[mlen].litlen = llen;
						opt[mlen].price = cost;
						//DEBUGLOG(7, "rPos:%3i => price:%3i (matchlen=%i) -- initial setup",
						//						mlen, cost, mlen);
					}
				}
				last_match_pos = firstMatch.len;
				{
					int addLit;
					for (addLit = 1; addLit <= TRAILING_LITERALS; addLit++) {
						opt[last_match_pos + addLit].mlen = 1; /* literal */
						opt[last_match_pos + addLit].off = 0;
						opt[last_match_pos + addLit].litlen = addLit;
						opt[last_match_pos + addLit].price = opt[last_match_pos].price + LZ4HC_literalsPrice(addLit);
						//DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i) -- initial setup",
						//						last_match_pos + addLit, opt[last_match_pos + addLit].price, addLit);
					}
				}

				/* check further positions */
				for (cur = 1; cur < last_match_pos; cur++) {
					byte* curPtr = ip + cur;
					LZ4HC_match_t newMatch;

					if (curPtr > mflimit) break;
					//DEBUGLOG(7, "rPos:%u[%u] vs [%u]%u",
					//				cur, opt[cur].price, opt[cur + 1].price, cur + 1);
					if (fullUpdate) {
						/* not useful to search here if next position has same (or lower) cost */
						if ((opt[cur + 1].price <= opt[cur].price)
							/* in some cases, next position has same cost, but cost rises sharply after, so a small match would still be beneficial */
							&& (opt[cur + 4].price < opt[cur].price + 3/*min seq price*/))
							continue;
					}
					else {
						/* not useful to search here if next position has same (or lower) cost */
						if (opt[cur + 1].price <= opt[cur].price) continue;
					}

					//DEBUGLOG(7, "search at rPos:%u", cur);
					if (fullUpdate)
						newMatch = LZ4HC_FindLongerMatch(ctx, curPtr, matchlimit, 4 - 1, nbSearches, dict, favorDecSpeed);
					else
						/* only test matches of minimum length; slightly faster, but misses a few bytes */
						newMatch = LZ4HC_FindLongerMatch(ctx, curPtr, matchlimit, last_match_pos - cur, nbSearches, dict, favorDecSpeed);
					if (newMatch.len == 0) continue;

					if (((size_t)newMatch.len > sufficient_len)
						|| (newMatch.len + cur >= LZ4_OPT_NUM)) {
						/* immediate encoding */
						best_mlen = newMatch.len;
						best_off = newMatch.off;
						last_match_pos = cur + 1;
						goto encode;
					}

					/* before match : set price with literals at beginning */
					{
						int baseLitlen = opt[cur].litlen;
						int litlen;
						for (litlen = 1; litlen < 4; litlen++) {
							int price = opt[cur].price - LZ4HC_literalsPrice(baseLitlen) + LZ4HC_literalsPrice(baseLitlen + litlen);
							int pos = cur + litlen;
							if (price < opt[pos].price) {
								opt[pos].mlen = 1; /* literal */
								opt[pos].off = 0;
								opt[pos].litlen = baseLitlen + litlen;
								opt[pos].price = price;
								//DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i)",
								//						pos, price, opt[pos].litlen);
							}
						}
					}

					/* set prices using match at position = cur */
					{
						int matchML = newMatch.len;
						int ml = 4;

						Debug.Assert(cur + newMatch.len < LZ4_OPT_NUM);
						for (; ml <= matchML; ml++) {
							int pos = cur + ml;
							int offset = newMatch.off;
							int price;
							int ll;
							//DEBUGLOG(7, "testing price rPos %i (last_match_pos=%i)",
							//						pos, last_match_pos);
							if (opt[cur].mlen == 1) {
								ll = opt[cur].litlen;
								price = ((cur > ll) ? opt[cur - ll].price : 0)
											+ LZ4HC_sequencePrice(ll, ml);
							}
							else {
								ll = 0;
								price = opt[cur].price + LZ4HC_sequencePrice(0, ml);
							}

							Debug.Assert((uint)favorDecSpeed <= 1);
							if (pos > last_match_pos + TRAILING_LITERALS
							 || price <= opt[pos].price - (int)favorDecSpeed) {
								//DEBUGLOG(7, "rPos:%3i => price:%3i (matchlen=%i)",
								//						pos, price, ml);
								Debug.Assert(pos < LZ4_OPT_NUM);
								if ((ml == matchML)  /* last pos of last match */
									&& (last_match_pos < pos))
									last_match_pos = pos;
								opt[pos].mlen = ml;
								opt[pos].off = offset;
								opt[pos].litlen = ll;
								opt[pos].price = price;
							}
						}
					}
					/* complete following positions with literals */
					{
						int addLit;
						for (addLit = 1; addLit <= TRAILING_LITERALS; addLit++) {
							opt[last_match_pos + addLit].mlen = 1; /* literal */
							opt[last_match_pos + addLit].off = 0;
							opt[last_match_pos + addLit].litlen = addLit;
							opt[last_match_pos + addLit].price = opt[last_match_pos].price + LZ4HC_literalsPrice(addLit);
							//DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i)", last_match_pos + addLit, opt[last_match_pos + addLit].price, addLit);
						}
					}
				}  /* for (cur = 1; cur <= last_match_pos; cur++) */

				Debug.Assert(last_match_pos < LZ4_OPT_NUM + TRAILING_LITERALS);
				best_mlen = opt[last_match_pos].mlen;
				best_off = opt[last_match_pos].off;
				cur = last_match_pos - best_mlen;

			encode: /* cur, last_match_pos, best_mlen, best_off must be set */
				Debug.Assert(cur < LZ4_OPT_NUM);
				Debug.Assert(last_match_pos >= 1);  /* == 1 when only one candidate */
																						//DEBUGLOG(6, "reverse traversal, looking for shortest path (last_match_pos=%i)", last_match_pos);
				{
					int candidate_pos = cur;
					int selected_matchLength = best_mlen;
					int selected_offset = best_off;
					while (true) {  /* from end to beginning */
						int next_matchLength = opt[candidate_pos].mlen;  /* can be 1, means literal */
						int next_offset = opt[candidate_pos].off;
						//DEBUGLOG(7, "pos %i: sequence length %i", candidate_pos, selected_matchLength);
						opt[candidate_pos].mlen = selected_matchLength;
						opt[candidate_pos].off = selected_offset;
						selected_matchLength = next_matchLength;
						selected_offset = next_offset;
						if (next_matchLength > candidate_pos) break; /* last match elected, first match to encode */
						Debug.Assert(next_matchLength > 0);  /* can be 1, means literal */
						candidate_pos -= next_matchLength;
					}
				}

				/* encode all recorded sequences in order */
				{
					int rPos = 0;  /* relative position (to ip) */
					while (rPos < last_match_pos) {
						int ml = opt[rPos].mlen;
						int offset = opt[rPos].off;
						if (ml == 1) { ip++; rPos++; continue; }  /* literal; note: can end up with several literals, in which case, skip them */
						rPos += ml;
						Debug.Assert(ml >= 4);
						Debug.Assert((offset >= 1) && (offset <= LZ4_DISTANCE_MAX));
						opSaved = op;
						if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, ip - offset, limit, oend))   /* updates ip, op and anchor */
							goto _dest_overflow;
					}
				}
			}  /* while (ip <= mflimit) */

		_last_literals:
			/* Encode Last Literals */
			{
				size_t lastRunSize = (size_t)(iend - anchor);  /* literals */
				size_t litLength = (lastRunSize + 255 - ((1U << (8 - 4)) - 1)) / 255;
				size_t totalSize = 1 + litLength + lastRunSize;
				if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
				if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
					if (limit == limitedOutput_directive.limitedOutput) return 0;  /* Check output limit */
																																				 /* adapt lastRunSize to fill 'dst' */
					lastRunSize = (size_t)(oend - op) - 1;
					litLength = (lastRunSize + 255 - ((1U << (8 - 4)) - 1)) / 255;
					lastRunSize -= litLength;
				}
				ip = anchor + lastRunSize;

				if (lastRunSize >= ((1U << (8 - 4)) - 1)) {
					size_t accumulator = lastRunSize - ((1U << (8 - 4)) - 1);
					// NOTE: translation: add byte cast
					*op++ = (byte)(((1U << (8 - 4)) - 1) << 4);
					for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
					*op++ = (byte)accumulator;
				}
				else {
					*op++ = (byte)(lastRunSize << 4);
				}
				Unsafe.CopyBlock(op, anchor, lastRunSize);
				op += lastRunSize;
			}

			/* End */
			// NOTE: translation: changed char* to byte*
			*srcSizePtr = (int)(((byte*)ip) - source);
			// NOTE: translation: changed char* to byte*
			return (int)((byte*)op - dst);

		_dest_overflow:
			if (limit == limitedOutput_directive.fillOutput) {
				op = opSaved;  /* restore correct out pointer */
				goto _last_literals;
			}
			return 0;
		}

		private static unsafe int LZ4HC_InsertAndFindBestMatch(LZ4HC_CCtx_internal* hc4,   /* Index table will be updated */
																			byte* ip, byte* iLimit,
																			byte** matchpos,
																			int maxNbAttempts,
																			// NOTE: translation: changed int to bool
																			bool patternAnalysis,
																			dictCtx_directive dict) {
			byte* uselessPtr = ip;
			/* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
			 * but this won't be the case here, as we define iLowLimit==ip,
			 * so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
			return LZ4HC_InsertAndGetWiderMatch(hc4, ip, ip, iLimit, 4 - 1, matchpos, &uselessPtr, maxNbAttempts, patternAnalysis, false /*chainSwap*/, dict, HCfavor_e.favorCompressionRatio);
		}

		private static unsafe int
		LZ4HC_InsertAndGetWiderMatch(
				LZ4HC_CCtx_internal* hc4,
				 byte* ip,
				 byte* iLowLimit,
				 byte* iHighLimit,
				int longest,
				 byte** matchpos,
				 byte** startpos,
				 int maxNbAttempts,
				 // NOTE: translation: changed from int to bool
				 bool patternAnalysis,
				 // NOTE: translation: changed from int to bool
				 bool chainSwap,
				 dictCtx_directive dict,
				 HCfavor_e favorDecSpeed) {
			ushort* chainTable = hc4->chainTable;
			uint* HashTable = hc4->hashTable;
			LZ4HC_CCtx_internal* dictCtx = hc4->dictCtx;
			byte* baseAddr = hc4->baseAddr;
			uint dictLimit = hc4->dictLimit;
			byte* lowPrefixPtr = baseAddr + dictLimit;
			uint ipIndex = (uint)(ip - baseAddr);
			uint lowestMatchIndex = (hc4->lowLimit + (LZ4_DISTANCE_MAX + 1) > ipIndex) ? hc4->lowLimit : ipIndex - LZ4_DISTANCE_MAX;
			byte* dictBase = hc4->dictBase;
			int lookBackLength = (int)(ip - iLowLimit);
			int nbAttempts = maxNbAttempts;
			uint matchChainPos = 0;
			uint pattern = LZ4_read32(ip);
			uint matchIndex;
			repeat_state_e repeat = repeat_state_e.rep_untested;
			size_t srcPatternLength = 0;

			//DEBUGLOG(7, "LZ4HC_InsertAndGetWiderMatch");
			/* First Match */
			LZ4HC_Insert(hc4, ip);
			matchIndex = HashTable[LZ4HC_hashPtr(ip)];
			//DEBUGLOG(7, "First match at index %u / %u (lowestMatchIndex)",
			//						matchIndex, lowestMatchIndex);

			while ((matchIndex >= lowestMatchIndex) && (nbAttempts != 0)) {
				int matchLength = 0;
				nbAttempts--;
				Debug.Assert(matchIndex < ipIndex);
				if (favorDecSpeed != HCfavor_e.favorCompressionRatio && (ipIndex - matchIndex < 8)) {
					/* do nothing */
				}
				else if (matchIndex >= dictLimit) {   /* within current Prefix */
					byte* matchPtr = baseAddr + matchIndex;
					Debug.Assert(matchPtr >= lowPrefixPtr);
					Debug.Assert(matchPtr < ip);
					Debug.Assert(longest >= 1);
					if (LZ4_read16(iLowLimit + longest - 1) == LZ4_read16(matchPtr - lookBackLength + longest - 1)) {
						if (LZ4_read32(matchPtr) == pattern) {
							int back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, lowPrefixPtr) : 0;
							matchLength = 4 + (int)LZ4_count(ip + 4, matchPtr + 4, iHighLimit);
							matchLength -= back;
							if (matchLength > longest) {
								longest = matchLength;
								*matchpos = matchPtr + back;
								*startpos = ip + back;
							}
						}
					}
				}
				else {   /* lowestMatchIndex <= matchIndex < dictLimit */
					byte* matchPtr = dictBase + matchIndex;
					if (LZ4_read32(matchPtr) == pattern) {
						byte* dictStart = dictBase + hc4->lowLimit;
						int back = 0;
						byte* vLimit = ip + (dictLimit - matchIndex);
						if (vLimit > iHighLimit) vLimit = iHighLimit;
						matchLength = (int)LZ4_count(ip + 4, matchPtr + 4, vLimit) + 4;
						if ((ip + matchLength == vLimit) && (vLimit < iHighLimit))
							// NOTE: translation: add int cast
							matchLength += (int)LZ4_count(ip + matchLength, lowPrefixPtr, iHighLimit);
						back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictStart) : 0;
						matchLength -= back;
						if (matchLength > longest) {
							longest = matchLength;
							*matchpos = baseAddr + matchIndex + back;   /* virtual pos, relative to ip, to retrieve offset */
							*startpos = ip + back;
						}
					}
				}

				if (chainSwap && matchLength == longest) {    /* better match => select a better chain */
					Debug.Assert(lookBackLength == 0);   /* search forward only */
					if (matchIndex + (uint)longest <= ipIndex) {
						int kTrigger = 4;
						uint distanceToNextMatch = 1;
						int end = longest - 4 + 1;
						int step = 1;
						int accel = 1 << kTrigger;
						int pos;
						for (pos = 0; pos < end; pos += step) {
							uint candidateDist = chainTable[(ushort)(matchIndex + (uint)pos)];
							step = (accel++ >> kTrigger);
							if (candidateDist > distanceToNextMatch) {
								distanceToNextMatch = candidateDist;
								matchChainPos = (uint)pos;
								accel = 1 << kTrigger;
							}
						}
						if (distanceToNextMatch > 1) {
							if (distanceToNextMatch > matchIndex) break;   /* avoid overflow */
							matchIndex -= distanceToNextMatch;
							continue;
						}
					}
				}

				{
					uint distNextMatch = chainTable[(ushort)(matchIndex)];
					if (patternAnalysis && distNextMatch == 1 && matchChainPos == 0) {
						uint matchCandidateIdx = matchIndex - 1;
						/* may be a repeated pattern */
						if (repeat == repeat_state_e.rep_untested) {
							if (((pattern & 0xFFFF) == (pattern >> 16))
								& ((pattern & 0xFF) == (pattern >> 24))) {
								repeat = repeat_state_e.rep_confirmed;
								// NOTE: translation: changed sizeof(pattern) to 4
								srcPatternLength = LZ4HC_countPattern(ip + 4, iHighLimit, pattern) + 4;
							}
							else {
								repeat = repeat_state_e.rep_not;
							}
						}
						if ((repeat == repeat_state_e.rep_confirmed) && (matchCandidateIdx >= lowestMatchIndex)
							&& LZ4HC_protectDictEnd(dictLimit, matchCandidateIdx)) {
							// NOTE: translation: changed int to bool
							bool extDict = matchCandidateIdx < dictLimit;
							byte* matchPtr = (extDict ? dictBase : baseAddr) + matchCandidateIdx;
							if (LZ4_read32(matchPtr) == pattern) {  /* good candidate */
								byte* dictStart = dictBase + hc4->lowLimit;
								byte* iLimit = extDict ? dictBase + dictLimit : iHighLimit;
								// NOTE: translation: changed sizeof(pattern) to 4
								size_t forwardPatternLength = LZ4HC_countPattern(matchPtr + 4, iLimit, pattern) + 4;
								if (extDict && matchPtr + forwardPatternLength == iLimit) {
									uint rotatedPattern = LZ4HC_rotatePattern(forwardPatternLength, pattern);
									forwardPatternLength += LZ4HC_countPattern(lowPrefixPtr, iHighLimit, rotatedPattern);
								}
								{
									byte* lowestMatchPtr = extDict ? dictStart : lowPrefixPtr;
									size_t backLength = LZ4HC_reverseCountPattern(matchPtr, lowestMatchPtr, pattern);
									size_t currentSegmentLength;
									if (!extDict && matchPtr - backLength == lowPrefixPtr && hc4->lowLimit < dictLimit) {
										uint rotatedPattern = LZ4HC_rotatePattern((uint)(-(int)backLength), pattern);
										backLength += LZ4HC_reverseCountPattern(dictBase + dictLimit, dictStart, rotatedPattern);
									}
									/* Limit backLength not go further than lowestMatchIndex */
									backLength = matchCandidateIdx - Math.Max(matchCandidateIdx - (uint)backLength, lowestMatchIndex);
									Debug.Assert(matchCandidateIdx - backLength >= lowestMatchIndex);
									currentSegmentLength = backLength + forwardPatternLength;
									/* Adjust to end of pattern if the source pattern fits, otherwise the beginning of the pattern */
									if ((currentSegmentLength >= srcPatternLength)   /* current pattern segment large enough to contain full srcPatternLength */
										&& (forwardPatternLength <= srcPatternLength)) { /* haven't reached this position yet */
										uint newMatchIndex = matchCandidateIdx + (uint)forwardPatternLength - (uint)srcPatternLength;  /* best position, full pattern, might be followed by more match */
										if (LZ4HC_protectDictEnd(dictLimit, newMatchIndex))
											matchIndex = newMatchIndex;
										else {
											/* Can only happen if started in the prefix */
											Debug.Assert(newMatchIndex >= dictLimit - 3 && newMatchIndex < dictLimit && !extDict);
											matchIndex = dictLimit;
										}
									}
									else {
										uint newMatchIndex = matchCandidateIdx - (uint)backLength;   /* farthest position in current segment, will find a match of length currentSegmentLength + maybe some back */
										if (!LZ4HC_protectDictEnd(dictLimit, newMatchIndex)) {
											Debug.Assert(newMatchIndex >= dictLimit - 3 && newMatchIndex < dictLimit && !extDict);
											matchIndex = dictLimit;
										}
										else {
											matchIndex = newMatchIndex;
											if (lookBackLength == 0) {  /* no back possible */
												size_t maxML = Math.Min(currentSegmentLength, srcPatternLength);
												if ((size_t)longest < maxML) {
													Debug.Assert(baseAddr + matchIndex < ip);
													if (ip - (baseAddr + matchIndex) > LZ4_DISTANCE_MAX) break;
													Debug.Assert(maxML < 2 * (1U << 30));
													longest = (int)maxML;
													*matchpos = baseAddr + matchIndex;   /* virtual pos, relative to ip, to retrieve offset */
													*startpos = ip;
												}
												{
													uint distToNextPattern = chainTable[(ushort)(matchIndex)];
													if (distToNextPattern > matchIndex) break;  /* avoid overflow */
													matchIndex -= distToNextPattern;
												}
											}
										}
									}
								}
								continue;
							}
						}
					}
				}   /* PA optimization */

				/* follow current chain */
				matchIndex -= chainTable[(ushort)(matchIndex + matchChainPos)];

			}  /* while ((matchIndex>=lowestMatchIndex) && (nbAttempts)) */

			if (dict == dictCtx_directive.usingDictCtxHc
				&& nbAttempts != 0
				&& ipIndex - lowestMatchIndex < LZ4_DISTANCE_MAX) {
				size_t dictEndOffset = (size_t)(dictCtx->end - dictCtx->baseAddr);
				uint dictMatchIndex = dictCtx->hashTable[LZ4HC_hashPtr(ip)];
				Debug.Assert(dictEndOffset <= 1 * (1U << 30));
				matchIndex = dictMatchIndex + lowestMatchIndex - (uint)dictEndOffset;
				while (ipIndex - matchIndex <= LZ4_DISTANCE_MAX && nbAttempts-- != 0) {
					byte* matchPtr = dictCtx->baseAddr + dictMatchIndex;

					if (LZ4_read32(matchPtr) == pattern) {
						int mlt;
						int back = 0;
						byte* vLimit = ip + (dictEndOffset - dictMatchIndex);
						if (vLimit > iHighLimit) vLimit = iHighLimit;
						mlt = (int)LZ4_count(ip + 4, matchPtr + 4, vLimit) + 4;
						back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictCtx->baseAddr + dictCtx->dictLimit) : 0;
						mlt -= back;
						if (mlt > longest) {
							longest = mlt;
							*matchpos = baseAddr + matchIndex + back;
							*startpos = ip + back;
						}
					}

					{
						uint nextOffset = dictCtx->chainTable[(ushort)(dictMatchIndex)];
						dictMatchIndex -= nextOffset;
						matchIndex -= nextOffset;
					}
				}
			}

			return longest;
		}

		private static unsafe int LZ4HC_countBack(byte* ip, byte* match,
												 byte* iMin, byte* mMin) {
			int back = 0;
			int min = (int)Math.Max(iMin - ip, mMin - match);
			Debug.Assert(min <= 0);
			Debug.Assert(ip >= iMin); Debug.Assert((size_t)(ip - iMin) < (1U << 31));
			Debug.Assert(match >= mMin); Debug.Assert((size_t)(match - mMin) < (1U << 31));
			while ((back > min)
					 && (ip[back - 1] == match[back - 1]))
				back--;
			return back;
		}

		// NOTE: translation: changed int to bool
		private static unsafe bool LZ4HC_protectDictEnd(uint dictLimit, uint matchIndex) {
			return ((uint)((dictLimit - 1) - matchIndex) >= 3);
		}

		private static unsafe uint LZ4HC_reverseCountPattern(byte* ip, byte* iLow, uint pattern) {
			byte* iStart = ip;

			while ((ip >= iLow + 4)) {
				if (LZ4_read32(ip - 4) != pattern) break;
				ip -= 4;
			}
			{
				byte* bytePtr = (byte*)(&pattern) + 3; /* works for any endianess */
				while ((ip > iLow)) {
					if (ip[-1] != *bytePtr) break;
					ip--; bytePtr--;
				}
			}
			return (uint)(iStart - ip);
		}

		private static unsafe uint LZ4HC_rotatePattern(size_t rotate, uint pattern) {
			// NOTE: translation: change sizeof(pattern) to 4
			size_t bitsToRotate = (rotate & (4 - 1)) << 3;
			if (bitsToRotate == 0)
				return pattern;
			return LZ4HC_rotl32(pattern, (int)bitsToRotate);
		}

		private static uint LZ4HC_rotl32(uint x, int r) { return ((x << r) | (x >> (32 - r))); }

		private static unsafe uint LZ4HC_countPattern(byte* ip, byte* iEnd, uint pattern32) {
			// NOTE: translation: changed reg_t to if
			if (IntPtr.Size == 8) {
				byte* iStart = ip;
				ulong pattern = (ulong)pattern32 + (((ulong)pattern32) << 32);

				while ((ip < iEnd - (8 - 1))) {
					ulong diff = LZ4_read_ARCH64(ip) ^ pattern;
					if (diff == 0) { ip += 8; continue; }
					ip += LZ4_NbCommonBytes64(diff);
					return (uint)(ip - iStart);
				}

				if (BitConverter.IsLittleEndian) {
					ulong patternByte = pattern;
					while ((ip < iEnd) && (*ip == (byte)patternByte)) {
						ip++; patternByte >>= 8;
					}
				}
				else {  /* big endian */
					int bitOffset = (8 * 8) - 8;
					while (ip < iEnd) {
						byte byteValue = (byte)(pattern >> bitOffset);
						if (*ip != byteValue) break;
						ip++; bitOffset -= 8;
					}
				}

				return (uint)(ip - iStart);
			}
			else if (IntPtr.Size == 4) {
				byte* iStart = ip;
				uint pattern = pattern32;

				while ((ip < iEnd - (4 - 1))) {
					uint diff = LZ4_read_ARCH32(ip) ^ pattern;
					if (diff == 0) { ip += 4; continue; }
					ip += LZ4_NbCommonBytes32(diff);
					return (uint)(ip - iStart);
				}

				if (BitConverter.IsLittleEndian) {
					uint patternByte = pattern;
					while ((ip < iEnd) && (*ip == (byte)patternByte)) {
						ip++; patternByte >>= 8;
					}
				}
				else {  /* big endian */
					int bitOffset = (4 * 8) - 8;
					while (ip < iEnd) {
						byte byteValue = (byte)(pattern >> bitOffset);
						if (*ip != byteValue) break;
						ip++; bitOffset -= 8;
					}
				}

				return (uint)(ip - iStart);
			}
			else {
				Debug.Fail("IntPtr.Size == " + IntPtr.Size);
				return 0;
			}
		}

		// NOTE: tranlsation: changed from int to bool
		private static unsafe bool LZ4HC_encodeSequence(
				 byte** ip,
				byte** op,
				 byte** anchor,
				int matchLength,
				 byte* match,
				limitedOutput_directive limit,
				byte* oend) {
			size_t length;
			byte* token = (*op)++;

			/* Encode Literal length */
			length = (size_t)(*ip - *anchor);
			if ((limit != limitedOutput_directive.notLimited) && ((*op + (length / 255) + length + (2 + 1 + LASTLITERALS)) > oend)) return true;   /* Check output limit */
			if (length >= ((1U << (8 - 4)) - 1)) {
				size_t len = length - ((1U << (8 - 4)) - 1);
				// NOTE: translation: add byte cast
				*token = (byte)(((1U << (8 - 4)) - 1) << 4);
				for (; len >= 255; len -= 255) *(*op)++ = 255;
				*(*op)++ = (byte)len;
			}
			else {
				*token = (byte)(length << 4);
			}

			/* Copy Literals */
			LZ4_wildCopy8(*op, *anchor, (*op) + length);
			*op += length;

			/* Encode Offset */
			Debug.Assert((*ip - match) <= LZ4_DISTANCE_MAX);   /* note : consider providing offset as a value, rather than as a pointer difference */
			LZ4_writeLE16(*op, (ushort)(*ip - match)); *op += 2;

			/* Encode MatchLength */
			Debug.Assert(matchLength >= 4);
			length = (size_t)matchLength - 4;
			if ((limit != limitedOutput_directive.notLimited) && (*op + (length / 255) + (1 + LASTLITERALS) > oend)) return true;   /* Check output limit */
			if (length >= ((1U << 4) - 1)) {
				// NOTE: translation: add byte cast
				*token += (byte)((1U << 4) - 1);
				length -= ((1U << 4) - 1);
				for (; length >= 510; length -= 510) { *(*op)++ = 255; *(*op)++ = 255; }
				if (length >= 255) { length -= 255; *(*op)++ = 255; }
				*(*op)++ = (byte)length;
			}
			else {
				*token += (byte)(length);
			}

			/* Prepare next loop */
			*ip += matchLength;
			*anchor = *ip;

			return false;
		}

		private static unsafe LZ4HC_match_t
		LZ4HC_FindLongerMatch(LZ4HC_CCtx_internal* ctx,
													 byte* ip, byte* iHighLimit,
													int minLen, int nbSearches,
													 dictCtx_directive dict,
													 HCfavor_e favorDecSpeed) {
			LZ4HC_match_t match = new LZ4HC_match_t() { off = 0, len = 0 };
			byte* matchPtr = null;
			/* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
			 * but this won't be the case here, as we define iLowLimit==ip,
			 * so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
			int matchLength = LZ4HC_InsertAndGetWiderMatch(ctx, ip, ip, iHighLimit, minLen, &matchPtr, &ip, nbSearches, true /*patternAnalysis*/, true /*chainSwap*/, dict, favorDecSpeed);
			if (matchLength <= minLen) return match;
			if (favorDecSpeed != HCfavor_e.favorCompressionRatio) {
				if ((matchLength > 18) & (matchLength <= 36)) matchLength = 18;   /* favor shortcut */
			}
			match.len = matchLength;
			match.off = (int)(ip - matchPtr);
			return match;
		}

		private static unsafe int LZ4HC_sequencePrice(int litlen, int mlen) {
			int price = 1 + 2; /* token + 16-bit offset */
			Debug.Assert(litlen >= 0);
			Debug.Assert(mlen >= 4);

			price += LZ4HC_literalsPrice(litlen);

			if (mlen >= (int)(((1U << 4) - 1) + 4))
				price += 1 + ((mlen - (int)(((1U << 4) - 1) + 4)) / 255);

			return price;
		}

		private static unsafe int LZ4HC_literalsPrice(int litlen) {
			int price = litlen;
			Debug.Assert(litlen >= 0);
			if (litlen >= (int)((1U << (8 - 4)) - 1))
				price += 1 + ((litlen - (int)((1U << (8 - 4)) - 1)) / 255);
			return price;
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_setStreamDecode(LZ4_streamDecode* LZ4_streamDecode, byte* dictionary, int dictSize) {
			LZ4_streamDecode_t_internal* lz4sd = &LZ4_streamDecode->internal_donotuse;
			lz4sd->prefixSize = (size_t)dictSize;
			lz4sd->prefixEnd = (byte*)dictionary + dictSize;
			lz4sd->externalDict = null;
			lz4sd->extDictSize = 0;
			return 1;
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_decompress_safe_continue(LZ4_streamDecode* LZ4_streamDecode, byte* source, byte* dest, int compressedSize, int maxOutputSize) {

			LZ4_streamDecode_t_internal* lz4sd = &LZ4_streamDecode->internal_donotuse;
			int result;

			if (lz4sd->prefixSize == 0) {
				/* The first call, no dictionary yet. */
				Debug.Assert(lz4sd->extDictSize == 0);
				result = LZ4_decompress_safe(source, dest, compressedSize, maxOutputSize);
				if (result <= 0) return result;
				lz4sd->prefixSize = (size_t)result;
				lz4sd->prefixEnd = (byte*)dest + result;
			}
			else if (lz4sd->prefixEnd == (byte*)dest) {
				/* They're rolling the current segment. */
				if (lz4sd->prefixSize >= 64 * (1 << 10) - 1)
					result = LZ4_decompress_safe_withPrefix64k(source, dest, compressedSize, maxOutputSize);

				else if (lz4sd->extDictSize == 0)
					result = LZ4_decompress_safe_withSmallPrefix(source, dest, compressedSize, maxOutputSize,
																											 lz4sd->prefixSize);
				else
					result = LZ4_decompress_safe_doubleDict(source, dest, compressedSize, maxOutputSize,
																									lz4sd->prefixSize, lz4sd->externalDict, lz4sd->extDictSize);
				if (result <= 0) return result;
				lz4sd->prefixSize += (size_t)result;
				lz4sd->prefixEnd += result;
			}
			else {
				/* The buffer wraps around, or they're switching to another buffer. */
				lz4sd->extDictSize = lz4sd->prefixSize;
				lz4sd->externalDict = lz4sd->prefixEnd - lz4sd->extDictSize;
				result = LZ4_decompress_safe_forceExtDict(source, dest, compressedSize, maxOutputSize,
																									lz4sd->externalDict, lz4sd->extDictSize);
				if (result <= 0) return result;
				lz4sd->prefixSize = (size_t)result;
				lz4sd->prefixEnd = (byte*)dest + result;
			}

			return result;
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_decompress_safe(byte* source, byte* dest, int compressedSize, int maxDecompressedSize) {
			return LZ4_decompress_generic(source, dest, compressedSize, maxDecompressedSize,
																	 endCondition_directive.endOnInputSize, earlyEnd_directive.decode_full_block, dict_directive.noDict,
																		(byte*)dest, null, 0);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_decompress_safe_withPrefix64k(byte* source, byte* dest, int compressedSize, int maxOutputSize) {
			return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
																		endCondition_directive.endOnInputSize, earlyEnd_directive.decode_full_block, dict_directive.withPrefix64k,
																		(byte*)dest - 64 * (1 << 10), null, 0);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_decompress_safe_withSmallPrefix(byte* source, byte* dest, int compressedSize, int maxOutputSize,
																							 size_t prefixSize) {
			return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
																		endCondition_directive.endOnInputSize, earlyEnd_directive.decode_full_block, dict_directive.noDict,
																		(byte*)dest - prefixSize, null, 0);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_decompress_safe_doubleDict(byte* source, byte* dest, int compressedSize, int maxOutputSize,
																	 size_t prefixSize, void* dictStart, size_t dictSize) {
			return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
																		endCondition_directive.endOnInputSize, earlyEnd_directive.decode_full_block, dict_directive.usingExtDict,
																		(byte*)dest - prefixSize, (byte*)dictStart, dictSize);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_decompress_safe_forceExtDict(byte* source, byte* dest,

																		 int compressedSize, int maxOutputSize,

																			void* dictStart, size_t dictSize) {
			return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
																		endCondition_directive.endOnInputSize, earlyEnd_directive.decode_full_block, dict_directive.usingExtDict,
																		(byte*)dest, (byte*)dictStart, dictSize);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_decompress_generic(
									byte* src,
								 byte* dst,
								 int srcSize,
								 int outputSize,         /* If endOnInput==endOnInputSize, this value is `dstCapacity` */
								 endCondition_directive endOnInput,   /* endOnOutputSize, endOnInputSize */
								 earlyEnd_directive partialDecoding,  /* full, partial */
								 dict_directive dict,                 /* noDict, withPrefix64k, usingExtDict */
									byte* lowPrefix,  /* always <= dst, == dst when no prefix */
									byte* dictStart,  /* only if dict==usingExtDict */
									size_t dictSize         /* note : = 0 if noDict */
								 ) {
			if (src == null) { return -1; }

			{
				byte* ip = (byte*)src;
				byte* iend = ip + srcSize;

				byte* op = (byte*)dst;
				byte* oend = op + outputSize;
				byte* cpy;

				byte* dictEnd = (dictStart == null) ? null : dictStart + dictSize;

				// NOTE: translation: change to bool
				bool safeDecode = (endOnInput == endCondition_directive.endOnInputSize);
				bool checkOffset = ((safeDecode) && (dictSize < (int)(64 * (1 << 10))));


				/* Set up the "end" pointers for the shortcut. */
				byte* shortiend = iend - (endOnInput != endCondition_directive.endOnOutputSize ? 14 : 8) /*maxLL*/ - 2 /*offset*/;
				byte* shortoend = oend - (endOnInput != endCondition_directive.endOnOutputSize ? 14 : 8) /*maxLL*/ - 18 /*maxML*/;

				byte* match;
				size_t offset;
				uint token;
				size_t length;


				//DEBUGLOG(5, "LZ4_decompress_generic (srcSize:%i, dstSize:%i)", srcSize, outputSize);

				/* Special cases */
				Debug.Assert(lowPrefix <= op);
				if ((endOnInput != endCondition_directive.endOnOutputSize) && ((outputSize == 0))) {
					/* Empty output buffer */
					if (partialDecoding != earlyEnd_directive.decode_full_block) return 0;
					return ((srcSize == 1) && (*ip == 0)) ? 0 : -1;
				}
				if ((endOnInput == endCondition_directive.endOnOutputSize) && ((outputSize == 0))) { return (*ip == 0 ? 1 : -1); }
				if ((endOnInput != endCondition_directive.endOnOutputSize) && (srcSize == 0)) { return -1; }

				/* Main Loop : decode remaining sequences where output < FASTLOOP_SAFE_DISTANCE */
				while (true) {
					token = *ip++;
					length = token >> 4;  /* literal length */

					Debug.Assert(endOnInput == endCondition_directive.endOnOutputSize || ip <= iend); /* ip < iend before the increment */

					/* A two-stage shortcut for the most common case:
					 * 1) If the literal length is 0..14, and there is enough space,
					 * enter the shortcut and copy 16 bytes on behalf of the literals
					 * (in the fast mode, only 8 bytes can be safely copied this way).
					 * 2) Further if the match length is 4..18, copy 18 bytes in a similar
					 * manner; but we ensure that there's enough space in the output for
					 * those 18 bytes earlier, upon entering the shortcut (in other words,
					 * there is a combined check for both stages).
					 */
					if ((endOnInput != endCondition_directive.endOnOutputSize ? length != ((1U << (8 - 4)) - 1) : length <= 8)
						/* strictly "less than" on input, to re-enter the loop with at least one byte */
						&& ((endOnInput != endCondition_directive.endOnOutputSize ? ip < shortiend : true) & (op <= shortoend))) {
						/* Copy the literals */
						Unsafe.CopyBlock(op, ip, endOnInput != endCondition_directive.endOnOutputSize ? 16u : 8u);
						op += length; ip += length;

						/* The second stage: prepare for match copying, decode full info.
						 * If it doesn't work out, the info won't be wasted. */
						length = token & ((1U << 4) - 1); /* match length */
						offset = LZ4_readLE16(ip); ip += 2;
						match = op - offset;
						Debug.Assert(match <= op); /* check overflow */

						/* Do not deal with overlapping matches. */
						if ((length != ((1U << 4) - 1))
							&& (offset >= 8)
							&& (dict == dict_directive.withPrefix64k || match >= lowPrefix)) {
							/* Copy the match. */
							Unsafe.CopyBlock(op + 0, match + 0, 8);
							Unsafe.CopyBlock(op + 8, match + 8, 8);
							Unsafe.CopyBlock(op + 16, match + 16, 2);
							op += length + 4;
							/* Both stages worked, load the next token. */
							continue;
						}

						/* The second stage didn't work out, but the info is ready.
						 * Propel it right to the point of match copying. */
						goto _copy_match;
					}

					/* decode literal length */
					if (length == ((1U << (8 - 4)) - 1)) {
						variable_length_error error = variable_length_error.ok;
						length += read_variable_length(&ip, iend - ((1U << (8 - 4)) - 1), (int)endOnInput, (int)endOnInput, &error);
						if (error == variable_length_error.initial_error) { goto _output_error; }
						// NOTE: translation: changed uptrval
						if (safeDecode) {
							if (IntPtr.Size == 8) {
								if (((ulong)(op) + length < (ulong)(op))) { goto _output_error; } /* overflow detection */
								if (((ulong)(ip) + length < (ulong)(ip))) { goto _output_error; } /* overflow detection */
							}
							else if (IntPtr.Size == 4) {
								if (((uint)(op) + length < (uint)(op))) { goto _output_error; } /* overflow detection */
								if (((uint)(ip) + length < (uint)(ip))) { goto _output_error; } /* overflow detection */
							}
							else {
								Debug.Fail("IntPtr.Size == " + IntPtr.Size);
							}
						}
					}

					/* copy literals */
					cpy = op + length;
#if LZ4_FAST_DEC_LOOP
        safe_literal_copy:
#endif
					//LZ4_STATIC_ASSERT(MFLIMIT >= WILDCOPYLENGTH);
					if (((endOnInput != endCondition_directive.endOnOutputSize) && ((cpy > oend - MFLIMIT) || (ip + length > iend - (2 + 1 + LASTLITERALS))))
						|| ((endOnInput == endCondition_directive.endOnOutputSize) && (cpy > oend - WILDCOPYLENGTH))) {
						/* We've either hit the input parsing restriction or the output parsing restriction.
						 * If we've hit the input parsing condition then this must be the last sequence.
						 * If we've hit the output parsing condition then we are either using partialDecoding
						 * or we've hit the output parsing condition.
						 */
						if (partialDecoding != earlyEnd_directive.decode_full_block) {
							/* Since we are partial decoding we may be in this block because of the output parsing
							 * restriction, which is not valid since the output buffer is allowed to be undersized.
							 */
							Debug.Assert(endOnInput != endCondition_directive.endOnOutputSize);
							/* If we're in this block because of the input parsing condition, then we must be on the
							 * last sequence (or invalid), so we must check that we exactly consume the input.
							 */
							if ((ip + length > iend - (2 + 1 + LASTLITERALS)) && (ip + length != iend)) { goto _output_error; }
							Debug.Assert(ip + length <= iend);
							/* We are finishing in the middle of a literals segment.
							 * Break after the copy.
							 */
							if (cpy > oend) {
								cpy = oend;
								Debug.Assert(op <= oend);
								length = (size_t)(oend - op);
							}
							Debug.Assert(ip + length <= iend);
						}
						else {
							/* We must be on the last sequence because of the parsing limitations so check
							 * that we exactly regenerate the original size (must be exact when !endOnInput).
							 */
							if ((endOnInput == endCondition_directive.endOnOutputSize) && (cpy != oend)) { goto _output_error; }
							/* We must be on the last sequence (or invalid) because of the parsing limitations
							 * so check that we exactly consume the input and don't overrun the output buffer.
							 */
							if ((endOnInput != endCondition_directive.endOnOutputSize) && ((ip + length != iend) || (cpy > oend))) { goto _output_error; }
						}
						Buffer.MemoryCopy(ip, op, length, length);  /* supports overlapping memory regions, which only matters for in-place decompression scenarios */
						ip += length;
						op += length;
						/* Necessarily EOF when !partialDecoding. When partialDecoding
						 * it is EOF if we've either filled the output buffer or hit
						 * the input parsing restriction.
						 */
						if (partialDecoding == earlyEnd_directive.decode_full_block || (cpy == oend) || (ip == iend)) {
							break;
						}
					}
					else {
						LZ4_wildCopy8(op, ip, cpy);   /* may overwrite up to WILDCOPYLENGTH beyond cpy */
						ip += length; op = cpy;
					}

					/* get offset */
					offset = LZ4_readLE16(ip); ip += 2;
					match = op - offset;

					/* get matchlength */
					length = token & ((1U << 4) - 1);

				_copy_match:
					if (length == ((1U << 4) - 1)) {
						variable_length_error error = variable_length_error.ok;
						length += read_variable_length(&ip, iend - LASTLITERALS + 1, (int)endOnInput, 0, &error);
						if (error != variable_length_error.ok) goto _output_error;
						if (safeDecode) {
							// NOTE: translation: changed uptrval
							if (IntPtr.Size == 8) {
								if (((ulong)(op) + length < (ulong)op)) goto _output_error;   /* overflow detection */
							}
							else if (IntPtr.Size == 4) {
								if (((uint)(op) + length < (uint)op)) goto _output_error;   /* overflow detection */
							}
							else {
								Debug.Fail("IntPtr.Size == " + IntPtr.Size);
							}
						}

					}
					length += 4;

#if LZ4_FAST_DEC_LOOP
        safe_match_copy:
#endif
					if ((checkOffset) && ((match + dictSize < lowPrefix))) goto _output_error;   /* Error : offset outside buffers */
																																											 /* match starting within external dictionary */
					if ((dict == dict_directive.usingExtDict) && (match < lowPrefix)) {
						if ((op + length > oend - LASTLITERALS)) {
							if (partialDecoding != earlyEnd_directive.decode_full_block) length = Math.Min(length, (size_t)(oend - op));
							else goto _output_error;   /* doesn't respect parsing restriction */
						}

						if (length <= (size_t)(lowPrefix - match)) {
							/* match fits entirely within external dictionary : just copy */
							Buffer.MemoryCopy(dictEnd - (lowPrefix - match), op, length, length);
							op += length;
						}
						else {
							/* match stretches into both external dictionary and current block */
							size_t copySize = (size_t)(lowPrefix - match);
							size_t restSize = length - copySize;
							Unsafe.CopyBlock(op, dictEnd - copySize, copySize);
							op += copySize;
							if (restSize > (size_t)(op - lowPrefix)) {  /* overlap copy */
								byte* endOfMatch = op + restSize;

								byte* copyFrom = lowPrefix;
								while (op < endOfMatch) *op++ = *copyFrom++;
							}
							else {
								Unsafe.CopyBlock(op, lowPrefix, restSize);
								op += restSize;
							}
						}
						continue;
					}
					Debug.Assert(match >= lowPrefix);

					/* copy match within block */
					cpy = op + length;

					/* partialDecoding : may end anywhere within the block */
					Debug.Assert(op <= oend);
					if (partialDecoding != earlyEnd_directive.decode_full_block && (cpy > oend - MATCH_SAFEGUARD_DISTANCE)) {
						size_t mlen = Math.Min(length, (size_t)(oend - op));

						byte* matchEnd = match + mlen;
						byte* copyEnd = op + mlen;
						if (matchEnd > op) {   /* overlap copy */
							while (op < copyEnd) { *op++ = *match++; }
						}
						else {
							Unsafe.CopyBlock(op, match, mlen);
						}
						op = copyEnd;
						if (op == oend) { break; }
						continue;
					}

					if ((offset < 8)) {
						LZ4_write32(op, 0);   /* silence msan warning when offset==0 */
						op[0] = match[0];
						op[1] = match[1];
						op[2] = match[2];
						op[3] = match[3];
						match += inc32table[offset];
						Unsafe.CopyBlock(op + 4, match, 4);
						match -= dec64table[offset];
					}
					else {
						Unsafe.CopyBlock(op, match, 8);
						match += 8;
					}
					op += 8;

					if ((cpy > oend - MATCH_SAFEGUARD_DISTANCE)) {
						byte* oCopyLimit = oend - (WILDCOPYLENGTH - 1);
						if (cpy > oend - LASTLITERALS) { goto _output_error; } /* Error : last LASTLITERALS bytes must be literals (uncompressed) */
						if (op < oCopyLimit) {
							LZ4_wildCopy8(op, match, oCopyLimit);
							match += oCopyLimit - op;
							op = oCopyLimit;
						}
						while (op < cpy) { *op++ = *match++; }
					}
					else {
						Unsafe.CopyBlock(op, match, 8);
						if (length > 16) { LZ4_wildCopy8(op + 8, match + 8, cpy); }
					}
					op = cpy;   /* wildcopy correction */
				}

				/* end of decoding */
				if (endOnInput != endCondition_directive.endOnOutputSize) {
					// NOTE: translation: changed char* to byte*
					return (int)(((byte*)op) - dst);     /* Nb of output bytes decoded */
				}
				else {
					// NOTE: translation: changed char* to byte*
					return (int)(((byte*)ip) - src);   /* Nb of input bytes read */
				}

			/* Overflow error detected */
			_output_error:
				// NOTE: translation: changed char* to byte*
				return (int)(-(((byte*)ip) - src)) - 1;
			}
		}

		private static unsafe ushort LZ4_readLE16(void* memPtr) {
			if (BitConverter.IsLittleEndian) {
				return LZ4_read16(memPtr);
			}
			else {
				byte* p = (byte*)memPtr;
				return (ushort)((ushort)p[0] + (p[1] << 8));
			}
		}

		private static unsafe uint read_variable_length(byte** ip, byte* lencheck, int loop_check, int initial_check, variable_length_error* error) {
			uint length = 0;
			uint s;
			if (initial_check != 0 && ((*ip) >= lencheck)) {    /* overflow detection */
				*error = variable_length_error.initial_error;
				return length;
			}
			do {
				s = **ip;
				(*ip)++;
				length += s;
				if (loop_check != 0 && ((*ip) >= lencheck)) {    /* overflow detection */

					*error = variable_length_error.loop_error;
					return length;
				}
			} while (s == 255);

			return length;
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_compress_default(byte* src, byte* dst, int srcSize, int maxOutputSize) {
			return LZ4_compress_fast(src, dst, srcSize, maxOutputSize, 1);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compress_fast(byte* source, byte* dest, int inputSize, int maxOutputSize, int acceleration) {

			int result;
			LZ4_stream ctx;
			LZ4_stream* ctxPtr = &ctx;
			result = LZ4_compress_fast_extState(ctxPtr, source, dest, inputSize, maxOutputSize, acceleration);

			return result;
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compress_fast_extState(void* state, byte* source, byte* dest, int inputSize, int maxOutputSize, int acceleration) {

			LZ4_stream_t_internal* ctx = &LZ4_initStream(state, (uint)sizeof(LZ4_stream))->internal_donotuse;
			Debug.Assert(ctx != null);
			if (acceleration < 1) acceleration = ACCELERATION_DEFAULT;
			if (maxOutputSize >= LZ4_compressBound(inputSize)) {
				if (inputSize < LZ4_64Klimit) {
					return LZ4_compress_generic(ctx, source, dest, inputSize, null, 0, limitedOutput_directive.notLimited, tableType_t.byU16, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
				}
				else {
					// NOTE: translation: changed sizeof(void*) to IntPtr.Size && uptrval to uint 
					tableType_t tableType = ((IntPtr.Size == 4) && ((uint)source > LZ4_DISTANCE_MAX)) ? tableType_t.byPtr : tableType_t.byU32;
					return LZ4_compress_generic(ctx, source, dest, inputSize, null, 0, limitedOutput_directive.notLimited, tableType, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
				}
			}
			else {
				if (inputSize < LZ4_64Klimit) {
					return LZ4_compress_generic(ctx, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType_t.byU16, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
				}
				else {
					// NOTE: translation: changed sizeof(void*) to IntPtr.Size && uptrval to uint 
					tableType_t tableType = ((IntPtr.Size == 4) && ((uint)source > LZ4_DISTANCE_MAX)) ? tableType_t.byPtr : tableType_t.byU32;
					return LZ4_compress_generic(ctx, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
				}
			}
		}

		// NOTE: translation: changed char* to byte*
		internal static unsafe int LZ4_compress_HC(byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {
			LZ4_streamHC state;
			LZ4_streamHC* statePtr = &state;
			int cSize = LZ4_compress_HC_extStateHC(statePtr, src, dst, srcSize, dstCapacity, compressionLevel);
			return cSize;
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compress_HC_extStateHC(void* state, byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {

			LZ4_streamHC* ctx = LZ4_initStreamHC(state, (uint)sizeof(LZ4_streamHC));
			if (ctx == null) return 0;   /* init failure */
			return LZ4_compress_HC_extStateHC_fastReset(state, src, dst, srcSize, dstCapacity, compressionLevel);
		}

		// NOTE: translation: changed char* to byte*
		private static unsafe int LZ4_compress_HC_extStateHC_fastReset(void* state, byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {

			LZ4HC_CCtx_internal* ctx = &((LZ4_streamHC*)state)->internal_donotuse;
			if (((size_t)(state) & (sizeof(void*) - 1)) != 0) return 0;   /* Error : state is not aligned for pointers (32 or 64 bits) */
			LZ4_resetStreamHC_fast((LZ4_streamHC*)state, compressionLevel);
			LZ4HC_init_internal(ctx, (byte*)src);
			if (dstCapacity < LZ4_compressBound(srcSize))
				return LZ4HC_compress_generic(ctx, src, dst, &srcSize, dstCapacity, compressionLevel, limitedOutput_directive.limitedOutput);
			else
				return LZ4HC_compress_generic(ctx, src, dst, &srcSize, dstCapacity, compressionLevel, limitedOutput_directive.notLimited);
		}

		private static unsafe void LZ4_resetStreamHC_fast(LZ4_streamHC* LZ4_streamHCPtr, int compressionLevel) {
			//DEBUGLOG(4, "LZ4_resetStreamHC_fast(%p, %d)", LZ4_streamHCPtr, compressionLevel);
			if (LZ4_streamHCPtr->internal_donotuse.dirty != 0) {
				LZ4_initStreamHC(LZ4_streamHCPtr, (uint)sizeof(LZ4_streamHC));
			}
			else {
				/* preserve end - base : can trigger clearTable's threshold */
				// NOTE: translation: changed uptrval to if
				if (IntPtr.Size == 8)
					LZ4_streamHCPtr->internal_donotuse.end -= (ulong)LZ4_streamHCPtr->internal_donotuse.baseAddr;
				else if (IntPtr.Size == 4)
					LZ4_streamHCPtr->internal_donotuse.end -= (uint)LZ4_streamHCPtr->internal_donotuse.baseAddr;
				else
					Debug.Fail("IntPtr.Size == " + IntPtr.Size);
				LZ4_streamHCPtr->internal_donotuse.baseAddr = null;
				LZ4_streamHCPtr->internal_donotuse.dictCtx = null;
			}
			LZ4_setCompressionLevel(LZ4_streamHCPtr, compressionLevel);
		}
	}
}
