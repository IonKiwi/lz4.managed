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

   ***

	 This is a translation of the original lz4 sources to C#
   lz4.c / lz4hc.c / xxhash.c
	  - source repository : https://github.com/IonKiwi/lz4.managed
*/

#define LZ4_FAST_DEC_LOOP

using System;
using System.Buffers;
#if NET6_0_OR_GREATER
using System.Buffers.Binary;
#endif
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;
using System.Security.Cryptography;
using System.Text.RegularExpressions;

namespace IonKiwi.lz4 {
    internal static class lz4 {

        private const int MINMATCH = 4;
        private const int LZ4_MEMORY_USAGE = 14;
        internal const int LZ4_STREAM_MINSIZE = (1 << LZ4_MEMORY_USAGE) + 32;
        private const int LZ4_HASHLOG = LZ4_MEMORY_USAGE - 2;
        internal const int LZ4_HASH_SIZE_U32 = 1 << LZ4_HASHLOG;

        private const int LZ4_MAX_INPUT_SIZE = 0x7E000000;

        private static int HASH_UNIT = IntPtr.Size;
        private static int STEPSIZE = IntPtr.Size;

        private const int WILDCOPYLENGTH = 8;
        private const int LASTLITERALS = 5;
        private const int MFLIMIT = 12;
        private const int MATCH_SAFEGUARD_DISTANCE = ((2 * WILDCOPYLENGTH) - MINMATCH);

        private const int LZ4_ACCELERATION_DEFAULT = 1;
        private const int LZ4_ACCELERATION_MAX = 65537;

        internal const int LZ4_STREAMDECODE_MINSIZE = 32;

        private const int FASTLOOP_SAFE_DISTANCE = 64;

        internal const uint rvl_error32 = unchecked((uint)-1);
        internal const ulong rvl_error64 = unchecked((ulong)-1);

        private const uint KB = (1 << 10);
        private const uint MB = (1 << 20);
        private const ulong GB = (1U << 30);

        private const int LZ4_minLength = (MFLIMIT + 1);
        private const int LZ4_64Klimit = (int)(64 * KB) + (MFLIMIT - 1);
        private const int LZ4_skipTrigger = 6;
        private const int LZ4_DISTANCE_MAX = 65535;
        private const int LZ4_DISTANCE_ABSOLUTE_MAX = 65535;

        private const int ML_BITS = 4;
        private const int ML_MASK = ((1 << ML_BITS) - 1);
        private const int RUN_BITS = (8 - ML_BITS);
        private const int RUN_MASK = ((1 << RUN_BITS) - 1);

        private static uint[] inc32table = { 0, 1, 2, 1, 0, 4, 4, 4 };
        private static int[] dec64table = { 0, 0, 0, -1, -4, 1, 2, 3 };

        private const int LZ4HC_HASHSIZE = 4;

        private const int LZ4HC_CLEVEL_DEFAULT = 9;
        private const int LZ4HC_CLEVEL_MAX = 12;

        internal const int LZ4_STREAMHC_MINSIZE = 262200;

        private const int LZ4HC_HASH_LOG = 15;
        internal const int LZ4HC_HASHTABLESIZE = (1 << LZ4HC_HASH_LOG);

        private const int LZ4HC_DICTIONARY_LOGSIZE = 16;
        internal const int LZ4HC_MAXD = (1 << LZ4HC_DICTIONARY_LOGSIZE);

        private const int OPTIMAL_ML = ((ML_MASK - 1) + MINMATCH);
        private const int LZ4_OPT_NUM = (1 << 12);

        private const int LZ4MID_HASHSIZE = 8;
        private const int LZ4MID_HASHLOG = (LZ4HC_HASH_LOG - 1);
        private const int LZ4MID_HASHTABLESIZE = (1 << LZ4MID_HASHLOG);

        private const int TRAILING_LITERALS = 3;

        private static cParams_t[] k_clTable = {
            new() { strat= lz4hc_strat_e.lz4mid,   nbSearches=2,     targetLength = 16 },  /* 0, unused */
            new() { strat=lz4hc_strat_e.lz4mid,    nbSearches=2,     targetLength = 16 },  /* 1, unused */
            new() { strat=lz4hc_strat_e.lz4mid,    nbSearches=2,     targetLength = 16 },  /* 2 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=4,     targetLength = 16 },  /* 3 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=8,     targetLength = 16 },  /* 4 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=16,    targetLength = 16 },  /* 5 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=32,    targetLength = 16 },  /* 6 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=64,    targetLength = 16 },  /* 7 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=128,   targetLength = 16 },  /* 8 */
            new() { strat=lz4hc_strat_e.lz4hc,     nbSearches=256,   targetLength = 16 },  /* 9 */
            new() { strat=lz4hc_strat_e.lz4opt,    nbSearches=96,    targetLength = 64 },  /*10==LZ4HC_CLEVEL_OPT_MIN*/
            new() { strat= lz4hc_strat_e.lz4opt,   nbSearches=512,   targetLength = 128 },  /*11 */
            new() { strat=lz4hc_strat_e.lz4opt,    nbSearches=16384, targetLength = LZ4_OPT_NUM },  /* 12==LZ4HC_CLEVEL_MAX */
        };

        private const uint PRIME32_1 = 2654435761U;
        private const uint PRIME32_2 = 2246822519U;
        private const uint PRIME32_3 = 3266489917U;
        private const uint PRIME32_4 = 668265263U;
        private const uint PRIME32_5 = 374761393U;

        private sealed class AllocWrapper : IDisposable {

            private readonly GCHandle _handle;
            private readonly byte[] _buffer;
            private bool _disposed;

            private AllocWrapper(GCHandle handle, byte[] buffer) {
                _handle = handle;
                _buffer = buffer;
            }

            public bool IsDisposed {
                get { return _disposed; }
            }

            public static unsafe T* Alloc<T>(bool zero, out IDisposable disposable) where T : unmanaged {
                int size = sizeof(T);
                var memory = ArrayPool<byte>.Shared.Rent(size);
                var handle = GCHandle.Alloc(memory, GCHandleType.Pinned);
                var addr = handle.AddrOfPinnedObject();
                if (zero) {
                    Unsafe.InitBlock((byte*)addr, 0, (uint)size);
                }
                disposable = new AllocWrapper(handle, memory);
                return (T*)addr;
            }

            public void Dispose() {
                if (_disposed) {
                    return;
                }
                _handle.Free();
                ArrayPool<byte>.Shared.Return(_buffer);
                _disposed = true;
            }
        }

        internal static unsafe int LZ4_compress_default(byte* src, byte* dst, int srcSize, int maxOutputSize) {
            return LZ4_compress_fast(src, dst, srcSize, maxOutputSize, 1);
        }

        private static unsafe int LZ4_compress_fast(byte* source, byte* dest, int inputSize, int maxOutputSize, int acceleration) {
            int result;
            LZ4_stream ctx;
            LZ4_stream* ctxPtr = &ctx;
            result = LZ4_compress_fast_extState(ctxPtr, source, dest, inputSize, maxOutputSize, acceleration);

            return result;
        }

        private static unsafe int LZ4_compress_fast_extState(void* state, byte* source, byte* dest, int inputSize, int maxOutputSize, int acceleration) {
            LZ4_stream_t_internal* ctx = &LZ4_initStream(state, sizeof(LZ4_stream))->internal_donotuse;
            Debug.Assert(ctx != null);
            if (acceleration < 1) acceleration = LZ4_ACCELERATION_DEFAULT;
            if (acceleration > LZ4_ACCELERATION_MAX) acceleration = LZ4_ACCELERATION_MAX;
            if (maxOutputSize >= LZ4_compressBound(inputSize)) {
                if (inputSize < LZ4_64Klimit) {
                    return LZ4_compress_generic(ctx, source, dest, inputSize, null, 0, limitedOutput_directive.notLimited, tableType_t.byU16, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
                }
                else {
                    tableType_t tableType = ((sizeof(void*) == 4) && ((long)source > LZ4_DISTANCE_MAX)) ? tableType_t.byPtr : tableType_t.byU32;
                    return LZ4_compress_generic(ctx, source, dest, inputSize, null, 0, limitedOutput_directive.notLimited, tableType, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
                }
            }
            else {
                if (inputSize < LZ4_64Klimit) {
                    return LZ4_compress_generic(ctx, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType_t.byU16, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
                }
                else {
                    tableType_t tableType = ((sizeof(void*) == 4) && ((long)source > LZ4_DISTANCE_MAX)) ? tableType_t.byPtr : tableType_t.byU32;
                    return LZ4_compress_generic(ctx, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.noDict, dictIssue_directive.noDictIssue, acceleration);
                }
            }
        }

        private static unsafe int LZ4_compress_generic(
            LZ4_stream_t_internal* cctx,
            byte* src,
            byte* dst,
            int srcSize,
            int* inputConsumed, /* only written when outputDirective == fillOutput */
            int dstCapacity,
            limitedOutput_directive outputDirective,
            tableType_t tableType,
            dict_directive dictDirective,
            dictIssue_directive dictIssue,
            int acceleration) {

            //DEBUGLOG(5, "LZ4_compress_generic: srcSize=%i, dstCapacity=%i",
            //					srcSize, dstCapacity);

            if ((uint)srcSize > (uint)LZ4_MAX_INPUT_SIZE) { return 0; }  /* Unsupported srcSize, too large (or negative) */
            if (srcSize == 0) {   /* src == null supported if srcSize == 0 */
                if (outputDirective != limitedOutput_directive.notLimited && dstCapacity <= 0) return 0;  /* no output, can't write anything */
                //DEBUGLOG(5, "Generating an empty block");
                Debug.Assert(outputDirective == limitedOutput_directive.notLimited || dstCapacity >= 1);
                Debug.Assert(dst != null);
                dst[0] = 0;
                if (outputDirective == limitedOutput_directive.fillOutput) {
                    Debug.Assert(inputConsumed != null);
                    *inputConsumed = 0;
                }
                return 1;
            }
            Debug.Assert(src != null);

            return LZ4_compress_generic_validated(cctx, src, dst, srcSize,
                                    inputConsumed, /* only written into if outputDirective == fillOutput */
                                    dstCapacity, outputDirective,
                                    tableType, dictDirective, dictIssue, acceleration);
        }

        private static unsafe int LZ4_compress_generic_validated(
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
            byte* basePtr = (byte*)source - startIndex;
            byte* lowLimit;

            LZ4_stream_t_internal* dictCtx = (LZ4_stream_t_internal*)cctx->dictCtx;
            byte* dictionary =
                dictDirective == dict_directive.usingDictCtx ? dictCtx->dictionary : cctx->dictionary;
            uint dictSize =
                dictDirective == dict_directive.usingDictCtx ? dictCtx->dictSize : cctx->dictSize;
            uint dictDelta =
                (dictDirective == dict_directive.usingDictCtx) ? startIndex - dictCtx->currentOffset : 0;   /* make indexes in dictCtx comparable with indexes in current context */

            bool maybe_extMem = (dictDirective == dict_directive.usingExtDict) || (dictDirective == dict_directive.usingDictCtx);
            uint prefixIdxLimit = startIndex - dictSize;   /* used when dictDirective == dictSmall */
            byte* dictEnd = dictionary != null ? dictionary + dictSize : dictionary;
            byte* anchor = (byte*)source;
            byte* iend = ip + inputSize;
            byte* mflimitPlusOne = iend - MFLIMIT + 1;
            byte* matchlimit = iend - LASTLITERALS;

            /* the dictCtx currentOffset is indexed on the start of the dictionary,
             * while a dictionary in the current context precedes the currentOffset */
            byte* dictBase = (dictionary == null) ? null :
                                   (dictDirective == dict_directive.usingDictCtx) ?
                                    dictionary + dictSize - dictCtx->currentOffset :
                                    dictionary + dictSize - startIndex;

            byte* op = (byte*)dest;
            byte* olimit = op + maxOutputSize;

            uint offset = 0;
            uint forwardH;

            //DEBUGLOG(5, "LZ4_compress_generic_validated: srcSize=%i, tableType=%u", inputSize, tableType);
            Debug.Assert(ip != null);
            if (tableType == tableType_t.byU16) Debug.Assert(inputSize < LZ4_64Klimit);  /* Size too large (not within 64K limit) */
            if (tableType == tableType_t.byPtr) Debug.Assert(dictDirective == dict_directive.noDict);   /* only supported use case with byPtr */
            /* If init conditions are not met, we don't have to mark stream
             * as having dirty context, since no action was taken yet */
            if (outputDirective == limitedOutput_directive.fillOutput && maxOutputSize < 1) { return 0; } /* Impossible to store anything */
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
            cctx->tableType = (uint)tableType;

            if (inputSize < LZ4_minLength) goto _last_literals;        /* Input too small, no compression (all literals) */

            /* First byte */
            {
                uint h = LZ4_hashPosition(ip, tableType);
                if (tableType == tableType_t.byPtr) {
                    LZ4_putPositionOnHash(ip, h, cctx->hashTable, tableType_t.byPtr);
                }
                else {
                    LZ4_putIndexOnHash(startIndex, h, cctx->hashTable, tableType);
                }
            }
            ip++; forwardH = LZ4_hashPosition(ip, tableType);

            /* Main Loop */
            for (; ; )
            {
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

                        if (forwardIp > mflimitPlusOne) goto _last_literals;
                        Debug.Assert(ip < mflimitPlusOne);

                        match = LZ4_getPositionOnHash(h, cctx->hashTable, tableType);
                        forwardH = LZ4_hashPosition(forwardIp, tableType);
                        LZ4_putPositionOnHash(ip, h, cctx->hashTable, tableType);

                    } while ((match + LZ4_DISTANCE_MAX < ip)
                           || (LZ4_read32(match) != LZ4_read32(ip)));

                }
                else {   /* byU32, byU16 */

                    byte* forwardIp = ip;
                    int step = 1;
                    int searchMatchNb = acceleration << LZ4_skipTrigger;
                    do {
                        uint h = forwardH;
                        uint current = (uint)(forwardIp - basePtr);
                        uint matchIndex = LZ4_getIndexOnHash(h, cctx->hashTable, tableType);
                        Debug.Assert(matchIndex <= current);
                        Debug.Assert(forwardIp - basePtr < (long)(2 * GB - 1));
                        ip = forwardIp;
                        forwardIp += step;
                        step = (searchMatchNb++ >> LZ4_skipTrigger);

                        if (forwardIp > mflimitPlusOne) goto _last_literals;
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
                                match = basePtr + matchIndex;
                                lowLimit = (byte*)source;
                            }
                        }
                        else if (dictDirective == dict_directive.usingExtDict) {
                            if (matchIndex < startIndex) {
                                //DEBUGLOG(7, "extDict candidate: matchIndex=%5u  <  startIndex=%5u", matchIndex, startIndex);
                                Debug.Assert(startIndex - matchIndex >= MINMATCH);
                                Debug.Assert(dictBase != null);
                                match = dictBase + matchIndex;
                                lowLimit = dictionary;
                            }
                            else {
                                match = basePtr + matchIndex;
                                lowLimit = (byte*)source;
                            }
                        }
                        else {   /* single continuous memory segment */
                            match = basePtr + matchIndex;
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
                Debug.Assert(ip > anchor); /* this is always true as ip has been advanced before entering the main loop */
                if ((match > lowLimit) && (ip[-1] == match[-1])) {
                    do { ip--; match--; } while (((ip > anchor) & (match > lowLimit)) && (ip[-1] == match[-1]));
                }

                /* Encode Literals */
                {
                    uint litLength = (uint)(ip - anchor);
                    token = op++;
                    if ((outputDirective == limitedOutput_directive.limitedOutput) &&  /* Check output buffer overflow */
                        (op + litLength + (2 + 1 + LASTLITERALS) + (litLength / 255) > olimit)) {
                        return 0;   /* cannot compress within `dst` budget. Stored indexes in hash table are nonetheless fine */
                    }
                    if ((outputDirective == limitedOutput_directive.fillOutput) &&
                        (op + (litLength + 240) / 255 /* litlen */ + litLength /* literals */ + 2 /* offset */ + 1 /* token */ + MFLIMIT - MINMATCH /* min last literals so last match is <= end - MFLIMIT */ > olimit)) {
                        op--;
                        goto _last_literals;
                    }
                    if (litLength >= RUN_MASK) {
                        uint len = litLength - RUN_MASK;
                        *token = (RUN_MASK << ML_BITS);
                        for (; len >= 255; len -= 255) *op++ = 255;
                        *op++ = (byte)len;
                    }
                    else *token = (byte)(litLength << ML_BITS);

                    /* Copy Literals */
                    LZ4_wildCopy8(op, anchor, op + litLength);
                    op += litLength;
                    //DEBUGLOG(6, "seq.start:%i, literals=%u, match.start:%i",
                    //            (int)(anchor - (byte*)source), litLength, (int)(ip - (byte*)source));
                }

            _next_match:
                /* at this stage, the following variables must be correctly set :
                 * - ip : at start of LZ operation
                 * - match : at start of previous pattern occurrence; can be within current prefix, or within extDict
                 * - offset : if maybe_ext_memSegment==1 (constant)
                 * - lowLimit : must be == dictionary to mean "match is within extDict"; must be == source otherwise
                 * - token and *token : position to write 4-bits for match length; higher 4-bits for literal length supposed already written
                 */

                if ((outputDirective == limitedOutput_directive.fillOutput) &&
                    (op + 2 /* offset */ + 1 /* token */ + MFLIMIT - MINMATCH /* min last literals so last match is <= end - MFLIMIT */ > olimit)) {
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
                        matchCode = LZ4_count(ip + MINMATCH, match + MINMATCH, limit);
                        ip += (long)matchCode + MINMATCH;
                        if (ip == limit) {
                            uint more = LZ4_count(limit, (byte*)source, matchlimit);
                            matchCode += more;
                            ip += more;
                        }
                        //DEBUGLOG(6, "             with matchLength=%u starting in extDict", matchCode + MINMATCH);
                    }
                    else {
                        matchCode = LZ4_count(ip + MINMATCH, match + MINMATCH, matchlimit);
                        ip += (long)matchCode + MINMATCH;
                        //DEBUGLOG(6, "             with matchLength=%u", matchCode + MINMATCH);
                    }

                    if ((outputDirective != limitedOutput_directive.notLimited) &&    /* Check output buffer overflow */
                        (op + (1 + LASTLITERALS) + (matchCode + 240) / 255 > olimit)) {
                        if (outputDirective == limitedOutput_directive.fillOutput) {
                            /* Match description too long : reduce it */
                            uint newMatchCode = 15 /* in token */ - 1 /* to avoid needing a zero byte */ + ((uint)(olimit - op) - 1 - LASTLITERALS) * 255;
                            ip -= matchCode - newMatchCode;
                            Debug.Assert(newMatchCode < matchCode);
                            matchCode = newMatchCode;
                            if (ip <= filledIp) {
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
                    if (matchCode >= ML_MASK) {
                        *token += ML_MASK;
                        matchCode -= ML_MASK;
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
                {
                    uint h = LZ4_hashPosition(ip - 2, tableType);
                    if (tableType == tableType_t.byPtr) {
                        LZ4_putPositionOnHash(ip - 2, h, cctx->hashTable, tableType_t.byPtr);
                    }
                    else {
                        uint idx = (uint)((ip - 2) - basePtr);
                        LZ4_putIndexOnHash(idx, h, cctx->hashTable, tableType);
                    }
                }

                /* Test next position */
                if (tableType == tableType_t.byPtr) {

                    match = LZ4_getPosition(ip, cctx->hashTable, tableType);
                    LZ4_putPosition(ip, cctx->hashTable, tableType);
                    if ((match + LZ4_DISTANCE_MAX >= ip)
                      && (LZ4_read32(match) == LZ4_read32(ip))) { token = op++; *token = 0; goto _next_match; }

                }
                else {   /* byU32, byU16 */

                    uint h = LZ4_hashPosition(ip, tableType);
                    uint current = (uint)(ip - basePtr);
                    uint matchIndex = LZ4_getIndexOnHash(h, cctx->hashTable, tableType);
                    Debug.Assert(matchIndex < current);
                    if (dictDirective == dict_directive.usingDictCtx) {
                        if (matchIndex < startIndex) {
                            /* there was no match, try the dictionary */
                            Debug.Assert(tableType == tableType_t.byU32);
                            matchIndex = LZ4_getIndexOnHash(h, dictCtx->hashTable, tableType_t.byU32);
                            match = dictBase + matchIndex;
                            lowLimit = dictionary;   /* required for match length counter */
                            matchIndex += dictDelta;
                        }
                        else {
                            match = basePtr + matchIndex;
                            lowLimit = (byte*)source;  /* required for match length counter */
                        }
                    }
                    else if (dictDirective == dict_directive.usingExtDict) {
                        if (matchIndex < startIndex) {
                            Debug.Assert(dictBase != null);
                            match = dictBase + matchIndex;
                            lowLimit = dictionary;   /* required for match length counter */
                        }
                        else {
                            match = basePtr + matchIndex;
                            lowLimit = (byte*)source;   /* required for match length counter */
                        }
                    }
                    else {   /* single memory segment */
                        match = basePtr + matchIndex;
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
                        //            (int)(anchor - (byte*)source), 0, (int)(ip - (byte*)source));
                        goto _next_match;
                    }
                }

                /* Prepare next loop */
                forwardH = LZ4_hashPosition(++ip, tableType);

            }

        _last_literals:
            /* Encode Last Literals */
            {
                long lastRun = (long)(iend - anchor);
                if ((outputDirective != limitedOutput_directive.notLimited) &&  /* Check output buffer overflow */
                    (op + lastRun + 1 + ((lastRun + 255 - RUN_MASK) / 255) > olimit)) {
                    if (outputDirective == limitedOutput_directive.fillOutput) {
                        /* adapt lastRun to fill 'dst' */
                        Debug.Assert(olimit >= op);
                        lastRun = (long)(olimit - op) - 1/*token*/;
                        lastRun -= (lastRun + 256 - RUN_MASK) / 256;  /*additional length tokens*/
                    }
                    else {
                        Debug.Assert(outputDirective == limitedOutput_directive.limitedOutput);
                        return 0;   /* cannot compress within `dst` budget. Stored indexes in hash table are nonetheless fine */
                    }
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRun);
                if (lastRun >= RUN_MASK) {
                    long accumulator = lastRun - RUN_MASK;
                    *op++ = RUN_MASK << ML_BITS;
                    for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRun << ML_BITS);
                }
                System.Runtime.CompilerServices.Unsafe.CopyBlock(op, anchor, (uint)lastRun);
                ip = anchor + lastRun;
                op += lastRun;
            }

            if (outputDirective == limitedOutput_directive.fillOutput) {
                *inputConsumed = (int)(((byte*)ip) - source);
            }
            result = (int)(((byte*)op) - dest);
            Debug.Assert(result > 0);
            //DEBUGLOG(5, "LZ4_compress_generic: compressed %i bytes into %i bytes", inputSize, result);
            return result;
        }

        internal static unsafe int LZ4_decompress_safe(byte* source, byte* dest, int compressedSize, int maxDecompressedSize) {
            return LZ4_decompress_generic(source, dest, compressedSize, maxDecompressedSize,
                earlyEnd_directive.decode_full_block, dict_directive.noDict,
                (byte*)dest, null, 0);
        }

        private static unsafe int LZ4_decompress_generic(
            byte* src,
            byte* dst,
            int srcSize,
            int outputSize,         /* If endOnInput==endOnInputSize, this value is `dstCapacity` */

            earlyEnd_directive partialDecoding,  /* full, partial */
            dict_directive dict,                 /* noDict, withPrefix64k, usingExtDict */
            byte* lowPrefix,  /* always <= dst, == dst when no prefix */
            byte* dictStart,  /* only if dict==usingExtDict */
            uint dictSize         /* note : = 0 if noDict */
            ) {

            if (IntPtr.Size == 4) {
                return LZ4_decompress_generic32(src, dst, srcSize, outputSize, partialDecoding, dict, lowPrefix, dictStart, dictSize);
            }
            else {
                return LZ4_decompress_generic64(src, dst, srcSize, outputSize, partialDecoding, dict, lowPrefix, dictStart, dictSize);
            }
        }

        private static unsafe int LZ4_decompress_generic64(
                byte* src,
                byte* dst,
                int srcSize,
                int outputSize,         /* If endOnInput==endOnInputSize, this value is `dstCapacity` */

                earlyEnd_directive partialDecoding,  /* full, partial */
                dict_directive dict,                 /* noDict, withPrefix64k, usingExtDict */
                byte* lowPrefix,  /* always <= dst, == dst when no prefix */
                byte* dictStart,  /* only if dict==usingExtDict */
                ulong dictSize         /* note : = 0 if noDict */
                ) {
            if ((src == null) || (outputSize < 0)) { return -1; }

            {
                byte* ip = (byte*)src;
                byte* iend = ip + srcSize;

                byte* op = (byte*)dst;
                byte* oend = op + outputSize;
                byte* cpy;

                byte* dictEnd = (dictStart == null) ? null : dictStart + dictSize;

                bool checkOffset = (dictSize < (int)(64 * KB));


                /* Set up the "end" pointers for the shortcut. */
                byte* shortiend = iend - 14 /*maxLL*/ - 2 /*offset*/;
                byte* shortoend = oend - 14 /*maxLL*/ - 18 /*maxML*/;

                byte* match = null;
                ulong offset = 0;
                uint token = 0;
                ulong length = 0;

                bool goto_safe_literal_copy = false;
                bool goto_safe_match_copy = false;


                //DEBUGLOG(5, "LZ4_decompress_generic (srcSize:%i, dstSize:%i)", srcSize, outputSize);

                /* Special cases */
                Debug.Assert(lowPrefix <= op);
                if (outputSize == 0) {
                    /* Empty output buffer */
                    if (partialDecoding != earlyEnd_directive.decode_full_block) return 0;
                    return ((srcSize == 1) && (*ip == 0)) ? 0 : -1;
                }
                if (srcSize == 0) { return -1; }

                /* LZ4_FAST_DEC_LOOP:
                 * designed for modern OoO performance cpus,
                 * where copying reliably 32-bytes is preferable to an unpredictable branch.
                 * note : fast loop may show a regression for some client arm chips. */
#if LZ4_FAST_DEC_LOOP
                if ((oend - op) < FASTLOOP_SAFE_DISTANCE) {
                    //DEBUGLOG(6, "move to safe decode loop");
                    goto safe_decode;
                }

                /* Fast loop : decode sequences as long as output < oend-FASTLOOP_SAFE_DISTANCE */
                //DEBUGLOG(6, "using fast decode loop");
                while (true) {
                    /* Main fastloop assertion: We can always wildcopy FASTLOOP_SAFE_DISTANCE */
                    Debug.Assert(oend - op >= FASTLOOP_SAFE_DISTANCE);
                    Debug.Assert(ip < iend);
                    token = *ip++;
                    length = token >> ML_BITS;  /* literal length */
                    //DEBUGLOG(7, "blockPos%6u: litLength token = %u", (uint)(op-(byte*)dst), (uint)length);

                    /* decode literal length */
                    if (length == RUN_MASK) {
                        ulong addl = read_variable_length64(&ip, iend - RUN_MASK, true);
                        if (addl == rvl_error64) {
                            //DEBUGLOG(6, "error reading long literal length");
                            goto _output_error;
                        }
                        length += addl;
                        if ((ulong)(op) + length < (ulong)(op)) { goto _output_error; } /* overflow detection */
                        if ((ulong)(ip) + length < (ulong)(ip)) { goto _output_error; } /* overflow detection */

                        /* copy literals */
                        //LZ4_STATIC_ASSERT(MFLIMIT >= WILDCOPYLENGTH);
                        if ((op + length > oend - 32) || (ip + length > iend - 32)) { goto_safe_literal_copy = true; goto safe_literal_copy_pre; }
                        LZ4_wildCopy32(op, ip, op + length);
                        ip += length; op += length;
                    }
                    else if (ip <= iend - (16 + 1/*max lit + offset + nextToken*/)) {
                        /* We don't need to check oend, since we check it once for each loop below */
                        //DEBUGLOG(7, "copy %u bytes in a 16-bytes stripe", (uint)length);
                        /* Literals can only be <= 14, but hope compilers optimize better when copy by a register size */
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, ip, 16);
                        ip += length; op += length;
                    }
                    else {
                        goto_safe_literal_copy = true;
                        goto safe_literal_copy_pre;
                    }

                    /* get offset */
                    offset = LZ4_readLE16(ip); ip += 2;
                    //DEBUGLOG(6, "blockPos%6u: offset = %u", (uint)(op-(byte*)dst), (uint)offset);
                    match = op - offset;
                    Debug.Assert(match <= op);  /* overflow check */

                    /* get matchlength */
                    length = token & ML_MASK;
                    //DEBUGLOG(7, "  match length token = %u (len==%u)", (uint)length, (uint)length+MINMATCH);

                    if (length == ML_MASK) {
                        ulong addl = read_variable_length64(&ip, iend - LASTLITERALS + 1, false);
                        if (addl == rvl_error64) {
                            //DEBUGLOG(5, "error reading long match length");
                            goto _output_error;
                        }
                        length += addl;
                        length += MINMATCH;
                        //DEBUGLOG(7, "  long match length == %u", (uint)length);
                        if ((ulong)(op) + length < (ulong)op) { goto _output_error; } /* overflow detection */
                        if (op + length >= oend - FASTLOOP_SAFE_DISTANCE) {
                            goto_safe_match_copy = true;
                            goto safe_match_copy_pre;
                        }
                    }
                    else {
                        length += MINMATCH;
                        if (op + length >= oend - FASTLOOP_SAFE_DISTANCE) {
                            //DEBUGLOG(7, "moving to safe_match_copy (ml==%u)", (uint)length);
                            goto_safe_match_copy = true;
                            goto safe_match_copy_pre;
                        }

                        /* Fastpath check: skip LZ4_wildCopy32 when true */
                        if ((dict == dict_directive.withPrefix64k) || (match >= lowPrefix)) {
                            if (offset >= 8) {
                                Debug.Assert(match >= lowPrefix);
                                Debug.Assert(match <= op);
                                Debug.Assert(op + 18 <= oend);

                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 8, match + 8, 8);
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 16, match + 16, 2);
                                op += length;
                                continue;
                            }
                        }
                    }

                    if (checkOffset && (match + dictSize < lowPrefix)) {
                        //DEBUGLOG(5, "Error : pos=%zi, offset=%zi => outside buffers", op-lowPrefix, op-match);
                        goto _output_error;
                    }
                    /* match starting within external dictionary */
                    if ((dict == dict_directive.usingExtDict) && (match < lowPrefix)) {
                        Debug.Assert(dictEnd != null);
                        if (op + length > oend - LASTLITERALS) {
                            if (partialDecoding != earlyEnd_directive.decode_full_block) {
                                //DEBUGLOG(7, "partialDecoding: dictionary match, close to dstEnd");
                                length = Math.Min(length, (ulong)(oend - op));
                            }
                            else {
                                //DEBUGLOG(6, "end-of-block condition violated")
                                goto _output_error;
                            }
                        }

                        if (length <= (ulong)(lowPrefix - match)) {
                            /* match fits entirely within external dictionary : just copy */
                            Buffer.MemoryCopy(dictEnd - (lowPrefix - match), op, length, length);
                            op += length;
                        }
                        else {
                            /* match stretches into both external dictionary and current block */
                            ulong copySize = (ulong)(lowPrefix - match);
                            ulong restSize = length - copySize;
                            Buffer.MemoryCopy(dictEnd - copySize, op, copySize, copySize);
                            op += copySize;
                            if (restSize > (ulong)(op - lowPrefix)) {  /* overlap copy */
                                byte* endOfMatch = op + restSize;
                                byte* copyFrom = lowPrefix;
                                while (op < endOfMatch) { *op++ = *copyFrom++; }
                            }
                            else {
                                Buffer.MemoryCopy(lowPrefix, op, restSize, restSize);
                                op += restSize;
                            }
                        }
                        continue;
                    }

                    /* copy match within block */
                    cpy = op + length;

                    Debug.Assert((op <= oend) && (oend - op >= 32));
                    if (offset < 16) {
                        LZ4_memcpy_using_offset64(op, match, cpy, offset);
                    }
                    else {
                        LZ4_wildCopy32(op, match, cpy);
                    }

                    op = cpy;   /* wildcopy correction */
                }
            safe_decode:
#endif

            safe_literal_copy_pre:
            safe_match_copy_pre:

                /* Main Loop : decode remaining sequences where output < FASTLOOP_SAFE_DISTANCE */
                //DEBUGLOG(6, "using safe decode loop");
                while (true) {
                    if (goto_safe_literal_copy) { goto_safe_literal_copy = false; goto safe_literal_copy; }
                    if (goto_safe_match_copy) { goto_safe_match_copy = false; goto safe_match_copy; }

                    Debug.Assert(ip < iend);
                    token = *ip++;
                    length = token >> ML_BITS;  /* literal length */
                    //DEBUGLOG(7, "blockPos%6u: litLength token = %u", (uint)(op - (byte*)dst), (uint)length);

                    /* A two-stage shortcut for the most common case:
                     * 1) If the literal length is 0..14, and there is enough space,
                     * enter the shortcut and copy 16 bytes on behalf of the literals
                     * (in the fast mode, only 8 bytes can be safely copied this way).
                     * 2) Further if the match length is 4..18, copy 18 bytes in a similar
                     * manner; but we ensure that there's enough space in the output for
                     * those 18 bytes earlier, upon entering the shortcut (in other words,
                     * there is a combined check for both stages).
                     */
                    if ((length != RUN_MASK)
                      /* strictly "less than" on input, to re-enter the loop with at least one byte */
                      && ((ip < shortiend) & (op <= shortoend))) {
                        /* Copy the literals */
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, ip, 16);
                        op += length; ip += length;

                        /* The second stage: prepare for match copying, decode full info.
                         * If it doesn't work out, the info won't be wasted. */
                        length = token & ML_MASK; /* match length */
                        //DEBUGLOG(7, "blockPos%6u: matchLength token = %u (len=%u)", (uint)(op - (byte*)dst), (uint)length, (uint)length + 4);
                        offset = LZ4_readLE16(ip); ip += 2;
                        match = op - offset;
                        Debug.Assert(match <= op); /* check overflow */

                        /* Do not deal with overlapping matches. */
                        if ((length != ML_MASK)
                          && (offset >= 8)
                          && (dict == dict_directive.withPrefix64k || match >= lowPrefix)) {
                            /* Copy the match. */
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 0, match + 0, 8);
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 8, match + 8, 8);
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 16, match + 16, 2);
                            op += length + MINMATCH;
                            /* Both stages worked, load the next token. */
                            continue;
                        }

                        /* The second stage didn't work out, but the info is ready.
                         * Propel it right to the point of match copying. */
                        goto _copy_match;
                    }

                    /* decode literal length */
                    if (length == RUN_MASK) {
                        ulong addl = read_variable_length64(&ip, iend - RUN_MASK, true);
                        if (addl == rvl_error64) { goto _output_error; }
                        length += addl;
                        if ((ulong)(op) + length < (ulong)(op)) { goto _output_error; } /* overflow detection */
                        if ((ulong)(ip) + length < (ulong)(ip)) { goto _output_error; } /* overflow detection */
                    }

#if LZ4_FAST_DEC_LOOP
                safe_literal_copy:
#endif
                    /* copy literals */
                    cpy = op + length;

                    //LZ4_STATIC_ASSERT(MFLIMIT >= WILDCOPYLENGTH);
                    if ((cpy > oend - MFLIMIT) || (ip + length > iend - (2 + 1 + LASTLITERALS))) {
                        /* We've either hit the input parsing restriction or the output parsing restriction.
                         * In the normal scenario, decoding a full block, it must be the last sequence,
                         * otherwise it's an error (invalid input or dimensions).
                         * In partialDecoding scenario, it's necessary to ensure there is no buffer overflow.
                         */
                        if (partialDecoding != earlyEnd_directive.decode_full_block) {
                            /* Since we are partial decoding we may be in this block because of the output parsing
                             * restriction, which is not valid since the output buffer is allowed to be undersized.
                             */
                            //DEBUGLOG(7, "partialDecoding: copying literals, close to input or output end")
                            //DEBUGLOG(7, "partialDecoding: literal length = %u", (uint)length);
                            //DEBUGLOG(7, "partialDecoding: remaining space in dstBuffer : %i", (int)(oend - op));
                            //DEBUGLOG(7, "partialDecoding: remaining space in srcBuffer : %i", (int)(iend - ip));
                            /* Finishing in the middle of a literals segment,
                             * due to lack of input.
                             */
                            if (ip + length > iend) {
                                length = (ulong)(iend - ip);
                                cpy = op + length;
                            }
                            /* Finishing in the middle of a literals segment,
                             * due to lack of output space.
                             */
                            if (cpy > oend) {
                                cpy = oend;
                                Debug.Assert(op <= oend);
                                length = (ulong)(oend - op);
                            }
                        }
                        else {
                            /* We must be on the last sequence (or invalid) because of the parsing limitations
                             * so check that we exactly consume the input and don't overrun the output buffer.
                             */
                            if ((ip + length != iend) || (cpy > oend)) {
                                //DEBUGLOG(5, "should have been last run of literals")
                                //DEBUGLOG(5, "ip(%p) + length(%i) = %p != iend (%p)", (void*)ip, (int)length, (void*)(ip + length), (void*)iend);
                                //DEBUGLOG(5, "or cpy(%p) > (oend-MFLIMIT)(%p)", (void*)cpy, (void*)(oend - MFLIMIT));
                                //DEBUGLOG(5, "after writing %u bytes / %i bytes available", (uint)(op - (byte*)dst), outputSize);
                                goto _output_error;
                            }
                        }
                        Buffer.MemoryCopy(ip, op, length, length);  /* supports overlapping memory regions, for in-place decompression scenarios */
                        ip += length;
                        op += length;
                        /* Necessarily EOF when !partialDecoding.
                         * When partialDecoding, it is EOF if we've either
                         * filled the output buffer or
                         * can't proceed with reading an offset for following match.
                         */
                        if (partialDecoding == earlyEnd_directive.decode_full_block || (cpy == oend) || (ip >= (iend - 2))) {
                            break;
                        }
                    }
                    else {
                        LZ4_wildCopy8(op, ip, cpy);   /* can overwrite up to 8 bytes beyond cpy */
                        ip += length; op = cpy;
                    }

                    /* get offset */
                    offset = LZ4_readLE16(ip); ip += 2;
                    match = op - offset;

                    /* get matchlength */
                    length = token & ML_MASK;
                //DEBUGLOG(7, "blockPos%6u: matchLength token = %u", (uint)(op - (byte*)dst), (uint)length);

                _copy_match:
                    if (length == ML_MASK) {
                        ulong addl = read_variable_length64(&ip, iend - LASTLITERALS + 1, false);
                        if (addl == rvl_error64) { goto _output_error; }
                        length += addl;
                        if ((ulong)(op) + length < (ulong)op) goto _output_error;   /* overflow detection */
                    }
                    length += MINMATCH;

#if LZ4_FAST_DEC_LOOP
                safe_match_copy:
#endif
                    if ((checkOffset) && (match + dictSize < lowPrefix)) goto _output_error;   /* Error : offset outside buffers */
                    /* match starting within external dictionary */
                    if ((dict == dict_directive.usingExtDict) && (match < lowPrefix)) {
                        Debug.Assert(dictEnd != null);
                        if (op + length > oend - LASTLITERALS) {
                            if (partialDecoding != earlyEnd_directive.decode_full_block) length = Math.Min(length, (ulong)(oend - op));
                            else goto _output_error;   /* doesn't respect parsing restriction */
                        }

                        if (length <= (ulong)(lowPrefix - match)) {
                            /* match fits entirely within external dictionary : just copy */
                            Buffer.MemoryCopy(dictEnd - (lowPrefix - match), op, length, length);
                            op += length;
                        }
                        else {
                            /* match stretches into both external dictionary and current block */
                            ulong copySize = (ulong)(lowPrefix - match);
                            ulong restSize = length - copySize;
                            Buffer.MemoryCopy(dictEnd - copySize, op, copySize, copySize);
                            op += copySize;
                            if (restSize > (ulong)(op - lowPrefix)) {  /* overlap copy */
                                byte* endOfMatch = op + restSize;
                                byte* copyFrom = lowPrefix;
                                while (op < endOfMatch) *op++ = *copyFrom++;
                            }
                            else {
                                Buffer.MemoryCopy(lowPrefix, op, restSize, restSize);
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
                        ulong mlen = Math.Min(length, (ulong)(oend - op));
                        byte* matchEnd = match + mlen;
                        byte* copyEnd = op + mlen;
                        if (matchEnd > op) {   /* overlap copy */
                            while (op < copyEnd) { *op++ = *match++; }
                        }
                        else {
                            Buffer.MemoryCopy(match, op, mlen, mlen);
                        }
                        op = copyEnd;
                        if (op == oend) { break; }
                        continue;
                    }

                    if (offset < 8) {
                        LZ4_write32(op, 0);   /* silence msan warning when offset==0 */
                        op[0] = match[0];
                        op[1] = match[1];
                        op[2] = match[2];
                        op[3] = match[3];
                        match += inc32table[offset];
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 4, match, 4);
                        match -= dec64table[offset];
                    }
                    else {
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                        match += 8;
                    }
                    op += 8;

                    if (cpy > oend - MATCH_SAFEGUARD_DISTANCE) {
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
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                        if (length > 16) { LZ4_wildCopy8(op + 8, match + 8, cpy); }
                    }
                    op = cpy;   /* wildcopy correction */
                }

                /* end of decoding */
                //DEBUGLOG(5, "decoded %i bytes", (int)(((byte*)op) - dst));
                return (int)(((byte*)op) - dst);     /* Nb of output bytes decoded */

            /* Overflow error detected */
            _output_error:
                return (int)(-(((byte*)ip) - src)) - 1;
            }
        }

        private static unsafe int LZ4_decompress_generic32(
    byte* src,
    byte* dst,
    int srcSize,
    int outputSize,         /* If endOnInput==endOnInputSize, this value is `dstCapacity` */

    earlyEnd_directive partialDecoding,  /* full, partial */
    dict_directive dict,                 /* noDict, withPrefix64k, usingExtDict */
    byte* lowPrefix,  /* always <= dst, == dst when no prefix */
    byte* dictStart,  /* only if dict==usingExtDict */
    uint dictSize         /* note : = 0 if noDict */
    ) {
            if ((src == null) || (outputSize < 0)) { return -1; }

            {
                byte* ip = (byte*)src;
                byte* iend = ip + srcSize;

                byte* op = (byte*)dst;
                byte* oend = op + outputSize;
                byte* cpy;

                byte* dictEnd = (dictStart == null) ? null : dictStart + dictSize;

                bool checkOffset = (dictSize < (int)(64 * KB));


                /* Set up the "end" pointers for the shortcut. */
                byte* shortiend = iend - 14 /*maxLL*/ - 2 /*offset*/;
                byte* shortoend = oend - 14 /*maxLL*/ - 18 /*maxML*/;

                byte* match = null;
                uint offset = 0;
                uint token = 0;
                uint length = 0;

                bool goto_safe_literal_copy = false;
                bool goto_safe_match_copy = false;


                //DEBUGLOG(5, "LZ4_decompress_generic (srcSize:%i, dstSize:%i)", srcSize, outputSize);

                /* Special cases */
                Debug.Assert(lowPrefix <= op);
                if (outputSize == 0) {
                    /* Empty output buffer */
                    if (partialDecoding != earlyEnd_directive.decode_full_block) return 0;
                    return ((srcSize == 1) && (*ip == 0)) ? 0 : -1;
                }
                if (srcSize == 0) { return -1; }

                /* LZ4_FAST_DEC_LOOP:
                 * designed for modern OoO performance cpus,
                 * where copying reliably 32-bytes is preferable to an unpredictable branch.
                 * note : fast loop may show a regression for some client arm chips. */
#if LZ4_FAST_DEC_LOOP
                if ((oend - op) < FASTLOOP_SAFE_DISTANCE) {
                    //DEBUGLOG(6, "move to safe decode loop");
                    goto safe_decode;
                }

                /* Fast loop : decode sequences as long as output < oend-FASTLOOP_SAFE_DISTANCE */
                //DEBUGLOG(6, "using fast decode loop");
                while (true) {
                    /* Main fastloop assertion: We can always wildcopy FASTLOOP_SAFE_DISTANCE */
                    Debug.Assert(oend - op >= FASTLOOP_SAFE_DISTANCE);
                    Debug.Assert(ip < iend);
                    token = *ip++;
                    length = token >> ML_BITS;  /* literal length */
                    //DEBUGLOG(7, "blockPos%6u: litLength token = %u", (uint)(op-(byte*)dst), (uint)length);

                    /* decode literal length */
                    if (length == RUN_MASK) {
                        uint addl = read_variable_length32(&ip, iend - RUN_MASK, true);
                        if (addl == rvl_error32) {
                            //DEBUGLOG(6, "error reading long literal length");
                            goto _output_error;
                        }
                        length += addl;
                        if ((uint)(op) + length < (uint)(op)) { goto _output_error; } /* overflow detection */
                        if ((uint)(ip) + length < (uint)(ip)) { goto _output_error; } /* overflow detection */

                        /* copy literals */
                        //LZ4_STATIC_ASSERT(MFLIMIT >= WILDCOPYLENGTH);
                        if ((op + length > oend - 32) || (ip + length > iend - 32)) { goto_safe_literal_copy = true; goto safe_literal_copy_pre; }
                        LZ4_wildCopy32(op, ip, op + length);
                        ip += length; op += length;
                    }
                    else if (ip <= iend - (16 + 1/*max lit + offset + nextToken*/)) {
                        /* We don't need to check oend, since we check it once for each loop below */
                        //DEBUGLOG(7, "copy %u bytes in a 16-bytes stripe", (uint)length);
                        /* Literals can only be <= 14, but hope compilers optimize better when copy by a register size */
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, ip, 16);
                        ip += length; op += length;
                    }
                    else {
                        goto_safe_literal_copy = true;
                        goto safe_literal_copy_pre;
                    }

                    /* get offset */
                    offset = LZ4_readLE16(ip); ip += 2;
                    //DEBUGLOG(6, "blockPos%6u: offset = %u", (uint)(op-(byte*)dst), (uint)offset);
                    match = op - offset;
                    Debug.Assert(match <= op);  /* overflow check */

                    /* get matchlength */
                    length = token & ML_MASK;
                    //DEBUGLOG(7, "  match length token = %u (len==%u)", (uint)length, (uint)length+MINMATCH);

                    if (length == ML_MASK) {
                        uint addl = read_variable_length32(&ip, iend - LASTLITERALS + 1, false);
                        if (addl == rvl_error32) {
                            //DEBUGLOG(5, "error reading long match length");
                            goto _output_error;
                        }
                        length += addl;
                        length += MINMATCH;
                        //DEBUGLOG(7, "  long match length == %u", (uint)length);
                        if ((uint)(op) + length < (uint)op) { goto _output_error; } /* overflow detection */
                        if (op + length >= oend - FASTLOOP_SAFE_DISTANCE) {
                            goto_safe_match_copy = true;
                            goto safe_match_copy_pre;
                        }
                    }
                    else {
                        length += MINMATCH;
                        if (op + length >= oend - FASTLOOP_SAFE_DISTANCE) {
                            //DEBUGLOG(7, "moving to safe_match_copy (ml==%u)", (uint)length);
                            goto_safe_match_copy = true;
                            goto safe_match_copy_pre;
                        }

                        /* Fastpath check: skip LZ4_wildCopy32 when true */
                        if ((dict == dict_directive.withPrefix64k) || (match >= lowPrefix)) {
                            if (offset >= 8) {
                                Debug.Assert(match >= lowPrefix);
                                Debug.Assert(match <= op);
                                Debug.Assert(op + 18 <= oend);

                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 8, match + 8, 8);
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 16, match + 16, 2);
                                op += length;
                                continue;
                            }
                        }
                    }

                    if (checkOffset && (match + dictSize < lowPrefix)) {
                        //DEBUGLOG(5, "Error : pos=%zi, offset=%zi => outside buffers", op-lowPrefix, op-match);
                        goto _output_error;
                    }
                    /* match starting within external dictionary */
                    if ((dict == dict_directive.usingExtDict) && (match < lowPrefix)) {
                        Debug.Assert(dictEnd != null);
                        if (op + length > oend - LASTLITERALS) {
                            if (partialDecoding != earlyEnd_directive.decode_full_block) {
                                //DEBUGLOG(7, "partialDecoding: dictionary match, close to dstEnd");
                                length = Math.Min(length, (uint)(oend - op));
                            }
                            else {
                                //DEBUGLOG(6, "end-of-block condition violated")
                                goto _output_error;
                            }
                        }

                        if (length <= (uint)(lowPrefix - match)) {
                            /* match fits entirely within external dictionary : just copy */
                            Buffer.MemoryCopy(dictEnd - (lowPrefix - match), op, length, length);
                            op += length;
                        }
                        else {
                            /* match stretches into both external dictionary and current block */
                            uint copySize = (uint)(lowPrefix - match);
                            uint restSize = length - copySize;
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op, dictEnd - copySize, copySize);
                            op += copySize;
                            if (restSize > (uint)(op - lowPrefix)) {  /* overlap copy */
                                byte* endOfMatch = op + restSize;
                                byte* copyFrom = lowPrefix;
                                while (op < endOfMatch) { *op++ = *copyFrom++; }
                            }
                            else {
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op, lowPrefix, restSize);
                                op += restSize;
                            }
                        }
                        continue;
                    }

                    /* copy match within block */
                    cpy = op + length;

                    Debug.Assert((op <= oend) && (oend - op >= 32));
                    if (offset < 16) {
                        LZ4_memcpy_using_offset32(op, match, cpy, offset);
                    }
                    else {
                        LZ4_wildCopy32(op, match, cpy);
                    }

                    op = cpy;   /* wildcopy correction */
                }
            safe_decode:
#endif

            safe_literal_copy_pre:
            safe_match_copy_pre:

                /* Main Loop : decode remaining sequences where output < FASTLOOP_SAFE_DISTANCE */
                //DEBUGLOG(6, "using safe decode loop");
                while (true) {
                    if (goto_safe_literal_copy) { goto_safe_literal_copy = false; goto safe_literal_copy; }
                    if (goto_safe_match_copy) { goto_safe_match_copy = false; goto safe_match_copy; }

                    Debug.Assert(ip < iend);
                    token = *ip++;
                    length = token >> ML_BITS;  /* literal length */
                    //DEBUGLOG(7, "blockPos%6u: litLength token = %u", (uint)(op - (byte*)dst), (uint)length);

                    /* A two-stage shortcut for the most common case:
                     * 1) If the literal length is 0..14, and there is enough space,
                     * enter the shortcut and copy 16 bytes on behalf of the literals
                     * (in the fast mode, only 8 bytes can be safely copied this way).
                     * 2) Further if the match length is 4..18, copy 18 bytes in a similar
                     * manner; but we ensure that there's enough space in the output for
                     * those 18 bytes earlier, upon entering the shortcut (in other words,
                     * there is a combined check for both stages).
                     */
                    if ((length != RUN_MASK)
                      /* strictly "less than" on input, to re-enter the loop with at least one byte */
                      && ((ip < shortiend) & (op <= shortoend))) {
                        /* Copy the literals */
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, ip, 16);
                        op += length; ip += length;

                        /* The second stage: prepare for match copying, decode full info.
                         * If it doesn't work out, the info won't be wasted. */
                        length = token & ML_MASK; /* match length */
                        //DEBUGLOG(7, "blockPos%6u: matchLength token = %u (len=%u)", (uint)(op - (byte*)dst), (uint)length, (uint)length + 4);
                        offset = LZ4_readLE16(ip); ip += 2;
                        match = op - offset;
                        Debug.Assert(match <= op); /* check overflow */

                        /* Do not deal with overlapping matches. */
                        if ((length != ML_MASK)
                          && (offset >= 8)
                          && (dict == dict_directive.withPrefix64k || match >= lowPrefix)) {
                            /* Copy the match. */
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 0, match + 0, 8);
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 8, match + 8, 8);
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 16, match + 16, 2);
                            op += length + MINMATCH;
                            /* Both stages worked, load the next token. */
                            continue;
                        }

                        /* The second stage didn't work out, but the info is ready.
                         * Propel it right to the point of match copying. */
                        goto _copy_match;
                    }

                    /* decode literal length */
                    if (length == RUN_MASK) {
                        uint addl = read_variable_length32(&ip, iend - RUN_MASK, true);
                        if (addl == rvl_error32) { goto _output_error; }
                        length += addl;
                        if ((uint)(op) + length < (uint)(op)) { goto _output_error; } /* overflow detection */
                        if ((uint)(ip) + length < (uint)(ip)) { goto _output_error; } /* overflow detection */
                    }

#if LZ4_FAST_DEC_LOOP
                safe_literal_copy:
#endif
                    /* copy literals */
                    cpy = op + length;

                    //LZ4_STATIC_ASSERT(MFLIMIT >= WILDCOPYLENGTH);
                    if ((cpy > oend - MFLIMIT) || (ip + length > iend - (2 + 1 + LASTLITERALS))) {
                        /* We've either hit the input parsing restriction or the output parsing restriction.
                         * In the normal scenario, decoding a full block, it must be the last sequence,
                         * otherwise it's an error (invalid input or dimensions).
                         * In partialDecoding scenario, it's necessary to ensure there is no buffer overflow.
                         */
                        if (partialDecoding != earlyEnd_directive.decode_full_block) {
                            /* Since we are partial decoding we may be in this block because of the output parsing
                             * restriction, which is not valid since the output buffer is allowed to be undersized.
                             */
                            //DEBUGLOG(7, "partialDecoding: copying literals, close to input or output end")
                            //DEBUGLOG(7, "partialDecoding: literal length = %u", (uint)length);
                            //DEBUGLOG(7, "partialDecoding: remaining space in dstBuffer : %i", (int)(oend - op));
                            //DEBUGLOG(7, "partialDecoding: remaining space in srcBuffer : %i", (int)(iend - ip));
                            /* Finishing in the middle of a literals segment,
                             * due to lack of input.
                             */
                            if (ip + length > iend) {
                                length = (uint)(iend - ip);
                                cpy = op + length;
                            }
                            /* Finishing in the middle of a literals segment,
                             * due to lack of output space.
                             */
                            if (cpy > oend) {
                                cpy = oend;
                                Debug.Assert(op <= oend);
                                length = (uint)(oend - op);
                            }
                        }
                        else {
                            /* We must be on the last sequence (or invalid) because of the parsing limitations
                             * so check that we exactly consume the input and don't overrun the output buffer.
                             */
                            if ((ip + length != iend) || (cpy > oend)) {
                                //DEBUGLOG(5, "should have been last run of literals")
                                //DEBUGLOG(5, "ip(%p) + length(%i) = %p != iend (%p)", (void*)ip, (int)length, (void*)(ip + length), (void*)iend);
                                //DEBUGLOG(5, "or cpy(%p) > (oend-MFLIMIT)(%p)", (void*)cpy, (void*)(oend - MFLIMIT));
                                //DEBUGLOG(5, "after writing %u bytes / %i bytes available", (uint)(op - (byte*)dst), outputSize);
                                goto _output_error;
                            }
                        }
                        Buffer.MemoryCopy(ip, op, length, length);  /* supports overlapping memory regions, for in-place decompression scenarios */
                        ip += length;
                        op += length;
                        /* Necessarily EOF when !partialDecoding.
                         * When partialDecoding, it is EOF if we've either
                         * filled the output buffer or
                         * can't proceed with reading an offset for following match.
                         */
                        if (partialDecoding == earlyEnd_directive.decode_full_block || (cpy == oend) || (ip >= (iend - 2))) {
                            break;
                        }
                    }
                    else {
                        LZ4_wildCopy8(op, ip, cpy);   /* can overwrite up to 8 bytes beyond cpy */
                        ip += length; op = cpy;
                    }

                    /* get offset */
                    offset = LZ4_readLE16(ip); ip += 2;
                    match = op - offset;

                    /* get matchlength */
                    length = token & ML_MASK;
                //DEBUGLOG(7, "blockPos%6u: matchLength token = %u", (uint)(op - (byte*)dst), (uint)length);

                _copy_match:
                    if (length == ML_MASK) {
                        uint addl = read_variable_length32(&ip, iend - LASTLITERALS + 1, false);
                        if (addl == rvl_error32) { goto _output_error; }
                        length += addl;
                        if ((uint)(op) + length < (uint)op) goto _output_error;   /* overflow detection */
                    }
                    length += MINMATCH;

#if LZ4_FAST_DEC_LOOP
                safe_match_copy:
#endif
                    if ((checkOffset) && (match + dictSize < lowPrefix)) goto _output_error;   /* Error : offset outside buffers */
                    /* match starting within external dictionary */
                    if ((dict == dict_directive.usingExtDict) && (match < lowPrefix)) {
                        Debug.Assert(dictEnd != null);
                        if (op + length > oend - LASTLITERALS) {
                            if (partialDecoding != earlyEnd_directive.decode_full_block) length = Math.Min(length, (uint)(oend - op));
                            else goto _output_error;   /* doesn't respect parsing restriction */
                        }

                        if (length <= (uint)(lowPrefix - match)) {
                            /* match fits entirely within external dictionary : just copy */
                            Buffer.MemoryCopy(dictEnd - (lowPrefix - match), op, length, length);
                            op += length;
                        }
                        else {
                            /* match stretches into both external dictionary and current block */
                            uint copySize = (uint)(lowPrefix - match);
                            uint restSize = length - copySize;
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op, dictEnd - copySize, copySize);
                            op += copySize;
                            if (restSize > (uint)(op - lowPrefix)) {  /* overlap copy */
                                byte* endOfMatch = op + restSize;
                                byte* copyFrom = lowPrefix;
                                while (op < endOfMatch) *op++ = *copyFrom++;
                            }
                            else {
                                System.Runtime.CompilerServices.Unsafe.CopyBlock(op, lowPrefix, restSize);
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
                        uint mlen = Math.Min(length, (uint)(oend - op));
                        byte* matchEnd = match + mlen;
                        byte* copyEnd = op + mlen;
                        if (matchEnd > op) {   /* overlap copy */
                            while (op < copyEnd) { *op++ = *match++; }
                        }
                        else {
                            System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, mlen);
                        }
                        op = copyEnd;
                        if (op == oend) { break; }
                        continue;
                    }

                    if (offset < 8) {
                        LZ4_write32(op, 0);   /* silence msan warning when offset==0 */
                        op[0] = match[0];
                        op[1] = match[1];
                        op[2] = match[2];
                        op[3] = match[3];
                        match += inc32table[offset];
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op + 4, match, 4);
                        match -= dec64table[offset];
                    }
                    else {
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                        match += 8;
                    }
                    op += 8;

                    if (cpy > oend - MATCH_SAFEGUARD_DISTANCE) {
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
                        System.Runtime.CompilerServices.Unsafe.CopyBlock(op, match, 8);
                        if (length > 16) { LZ4_wildCopy8(op + 8, match + 8, cpy); }
                    }
                    op = cpy;   /* wildcopy correction */
                }

                /* end of decoding */
                //DEBUGLOG(5, "decoded %i bytes", (int)(((byte*)op) - dst));
                return (int)(((byte*)op) - dst);     /* Nb of output bytes decoded */

            /* Overflow error detected */
            _output_error:
                return (int)(-(((byte*)ip) - src)) - 1;
            }
        }

        private static unsafe uint LZ4_hashPosition(void* p, tableType_t tableType) {
            if ((IntPtr.Size == 8) && (tableType != tableType_t.byU16)) return LZ4_hash5(LZ4_read_ARCH64(p), tableType);
            return LZ4_hash4(LZ4_read32(p), tableType);
        }

        private static unsafe uint LZ4_hash4(uint sequence, tableType_t tableType) {
            if (tableType == tableType_t.byU16)
                return ((sequence * 2654435761U) >> ((MINMATCH * 8) - (LZ4_HASHLOG + 1)));
            else
                return ((sequence * 2654435761U) >> ((MINMATCH * 8) - LZ4_HASHLOG));
        }

        private static unsafe uint LZ4_hash5(ulong sequence, tableType_t tableType) {

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

        private static unsafe void LZ4_putPositionOnHash(byte* p, uint h, void* tableBase, tableType_t tableType) {
            byte** hashTable = (byte**)tableBase;
            Debug.Assert(tableType == tableType_t.byPtr); // (void)tableType;
            hashTable[h] = p;
        }

        private static unsafe void LZ4_putIndexOnHash(uint idx, uint h, void* tableBase, tableType_t tableType) {
            switch (tableType) {
                default: /* fallthrough */
                case tableType_t.clearedTable: /* fallthrough */
                case tableType_t.byPtr: { /* illegal! */ Debug.Assert(false); return; }
                case tableType_t.byU32: { uint* hashTable = (uint*)tableBase; hashTable[h] = idx; return; }
                case tableType_t.byU16: { ushort* hashTable = (ushort*)tableBase; Debug.Assert(idx < 65536); hashTable[h] = (ushort)idx; return; }
            }
        }

        private static unsafe void LZ4_putPosition(byte* p, void* tableBase, tableType_t tableType) {

            uint h = LZ4_hashPosition(p, tableType);
            LZ4_putPositionOnHash(p, h, tableBase, tableType);
        }

        private static unsafe byte* LZ4_getPositionOnHash(uint h, void* tableBase, tableType_t tableType) {
            Debug.Assert(tableType == tableType_t.byPtr); // (void)tableType;
            byte** hashTable = (byte**)tableBase; return hashTable[h];
        }

        private static unsafe byte* LZ4_getPosition(byte* p,
                             void* tableBase, tableType_t tableType) {
            uint h = LZ4_hashPosition(p, tableType);
            return LZ4_getPositionOnHash(h, tableBase, tableType);
        }

        private static unsafe uint LZ4_getIndexOnHash(uint h, void* tableBase, tableType_t tableType) {
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
            Debug.Assert(false); return 0;  /* forbidden case */
        }

        private static unsafe void LZ4_clearHash(uint h, void* tableBase, tableType_t tableType) {
            switch (tableType) {
                default: /* fallthrough */
                case tableType_t.clearedTable: { /* illegal! */ Debug.Assert(false); return; }
                case tableType_t.byPtr: { byte** hashTable = (byte**)tableBase; hashTable[h] = null; return; }
                case tableType_t.byU32: { uint* hashTable = (uint*)tableBase; hashTable[h] = 0; return; }
                case tableType_t.byU16: { ushort* hashTable = (ushort*)tableBase; hashTable[h] = 0; return; }
            }
        }

        private static unsafe ulong read_variable_length64(byte** ip, byte* ilimit,
                                     bool initial_check) {
            ulong s, length = 0;
            Debug.Assert(ip != null);
            Debug.Assert(*ip != null);
            Debug.Assert(ilimit != null);
            if (initial_check && ((*ip) >= ilimit)) {    /* read limit reached */
                return rvl_error64;
            }
            s = **ip;
            (*ip)++;
            length += s;
            if ((*ip) > ilimit) {    /* read limit reached */
                return rvl_error64;
            }
            if (s != 255) return length;
            do {
                s = **ip;
                (*ip)++;
                length += s;
                if ((*ip) > ilimit) {    /* read limit reached */
                    return rvl_error64;
                }
            } while (s == 255);

            return length;
        }

        private static unsafe uint read_variable_length32(byte** ip, byte* ilimit,
                                         bool initial_check) {
            uint s, length = 0;
            Debug.Assert(ip != null);
            Debug.Assert(*ip != null);
            Debug.Assert(ilimit != null);
            if (initial_check && ((*ip) >= ilimit)) {    /* read limit reached */
                return rvl_error32;
            }
            s = **ip;
            (*ip)++;
            length += s;
            if ((*ip) > ilimit) {    /* read limit reached */
                return rvl_error32;
            }
            /* accumulator overflow detection (32-bit mode only) */
            if (length > (unchecked((uint)(-1)) / 2)) {
                return rvl_error32;
            }
            if (s != 255) return length;
            do {
                s = **ip;
                (*ip)++;
                length += s;
                if ((*ip) > ilimit) {    /* read limit reached */
                    return rvl_error32;
                }
                /* accumulator overflow detection (32-bit mode only) */
                if (length > (unchecked((uint)(-1)) / 2)) {
                    return rvl_error32;
                }
            } while (s == 255);

            return length;
        }

        private static unsafe void LZ4_memcpy_using_offset64(byte* dstPtr, byte* srcPtr, byte* dstEnd, ulong offset) {
            byte* v = stackalloc byte[8];

            Debug.Assert(dstEnd >= dstPtr + MINMATCH);

            switch (offset) {
                case 1:
                    System.Runtime.CompilerServices.Unsafe.InitBlock(v, *srcPtr, 8);
                    break;
                case 2:
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(v, srcPtr, 2);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[2], srcPtr, 2);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[4], v, 4);
                    break;
                case 4:
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(v, srcPtr, 4);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[4], srcPtr, 4);
                    break;
                default:
                    LZ4_memcpy_using_offset_base64(dstPtr, srcPtr, dstEnd, offset);
                    return;
            }

            System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, v, 8);
            dstPtr += 8;
            while (dstPtr < dstEnd) {
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, v, 8);
                dstPtr += 8;
            }
        }

        private static unsafe void LZ4_memcpy_using_offset_base64(byte* dstPtr, byte* srcPtr, byte* dstEnd, ulong offset) {
            Debug.Assert(srcPtr + offset == dstPtr);
            if (offset < 8) {
                LZ4_write32(dstPtr, 0);   /* silence an msan warning when offset==0 */
                dstPtr[0] = srcPtr[0];
                dstPtr[1] = srcPtr[1];
                dstPtr[2] = srcPtr[2];
                dstPtr[3] = srcPtr[3];
                srcPtr += inc32table[offset];
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr + 4, srcPtr, 4);
                srcPtr -= dec64table[offset];
                dstPtr += 8;
            }
            else {
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, srcPtr, 8);
                dstPtr += 8;
                srcPtr += 8;
            }

            LZ4_wildCopy8(dstPtr, srcPtr, dstEnd);
        }

        private static unsafe void LZ4_memcpy_using_offset32(byte* dstPtr, byte* srcPtr, byte* dstEnd, uint offset) {
            byte* v = stackalloc byte[8];

            Debug.Assert(dstEnd >= dstPtr + MINMATCH);

            switch (offset) {
                case 1:
                    System.Runtime.CompilerServices.Unsafe.InitBlock(v, *srcPtr, 8);
                    break;
                case 2:
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(v, srcPtr, 2);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[2], srcPtr, 2);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[4], v, 4);
                    break;
                case 4:
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(v, srcPtr, 4);
                    System.Runtime.CompilerServices.Unsafe.CopyBlock(&v[4], srcPtr, 4);
                    break;
                default:
                    LZ4_memcpy_using_offset_base32(dstPtr, srcPtr, dstEnd, offset);
                    return;
            }

            System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, v, 8);
            dstPtr += 8;
            while (dstPtr < dstEnd) {
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, v, 8);
                dstPtr += 8;
            }
        }

        private static unsafe void LZ4_memcpy_using_offset_base32(byte* dstPtr, byte* srcPtr, byte* dstEnd, uint offset) {
            Debug.Assert(srcPtr + offset == dstPtr);
            if (offset < 8) {
                LZ4_write32(dstPtr, 0);   /* silence an msan warning when offset==0 */
                dstPtr[0] = srcPtr[0];
                dstPtr[1] = srcPtr[1];
                dstPtr[2] = srcPtr[2];
                dstPtr[3] = srcPtr[3];
                srcPtr += inc32table[offset];
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr + 4, srcPtr, 4);
                srcPtr -= dec64table[offset];
                dstPtr += 8;
            }
            else {
                System.Runtime.CompilerServices.Unsafe.CopyBlock(dstPtr, srcPtr, 8);
                dstPtr += 8;
                srcPtr += 8;
            }

            LZ4_wildCopy8(dstPtr, srcPtr, dstEnd);
        }

        private static unsafe void LZ4_wildCopy32(void* dstPtr, void* srcPtr, void* dstEnd) {
            byte* d = (byte*)dstPtr;
            byte* s = (byte*)srcPtr;
            byte* e = (byte*)dstEnd;

            do { System.Runtime.CompilerServices.Unsafe.CopyBlock(d, s, 16); System.Runtime.CompilerServices.Unsafe.CopyBlock(d + 16, s + 16, 16); d += 32; s += 32; } while (d < e);
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

        private static unsafe uint LZ4_read32(void* memPtr) {
            uint val; System.Runtime.CompilerServices.Unsafe.CopyBlock(&val, memPtr, 4); return val;
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
            System.Runtime.CompilerServices.Unsafe.CopyBlock(memPtr, &value, 2);
        }

        private static unsafe void LZ4_write32(void* memPtr, uint value) {
            System.Runtime.CompilerServices.Unsafe.CopyBlock(memPtr, &value, 4);
        }

        private static unsafe void LZ4_wildCopy8(void* dstPtr, void* srcPtr, void* dstEnd) {
            byte* d = (byte*)dstPtr;
            byte* s = (byte*)srcPtr;
            byte* e = (byte*)dstEnd;

            do { System.Runtime.CompilerServices.Unsafe.CopyBlock(d, s, 8); d += 8; s += 8; } while (d < e);
        }

        private static unsafe ushort LZ4_read16(void* memPtr) {
            ushort val; System.Runtime.CompilerServices.Unsafe.CopyBlock(&val, memPtr, 2); return val;
        }

        private static unsafe ulong LZ4_read_ARCH64(void* memPtr) {
            ulong val; System.Runtime.CompilerServices.Unsafe.CopyBlock(&val, memPtr, 8); return val;
        }

        private static unsafe uint LZ4_read_ARCH32(void* memPtr) {
            uint val; System.Runtime.CompilerServices.Unsafe.CopyBlock(&val, memPtr, 4); return val;
        }

        private static unsafe uint LZ4_count(byte* pIn, byte* pMatch, byte* pInLimit) {
            byte* pStart = pIn;

            if (IntPtr.Size == 4) {
                if (pIn < pInLimit - (STEPSIZE - 1)) {
                    uint diff = LZ4_read_ARCH32(pMatch) ^ LZ4_read_ARCH32(pIn);
                    if (diff == 0) {
                        pIn += STEPSIZE; pMatch += STEPSIZE;
                    }
                    else {
                        return LZ4_NbCommonBytes32(diff);
                    }
                }

                while (pIn < pInLimit - (STEPSIZE - 1)) {
                    uint diff = LZ4_read_ARCH32(pMatch) ^ LZ4_read_ARCH32(pIn);
                    if (diff == 0) { pIn += STEPSIZE; pMatch += STEPSIZE; continue; }
                    pIn += LZ4_NbCommonBytes32(diff);
                    return (uint)(pIn - pStart);
                }
            }
            else if (IntPtr.Size == 8) {
                if (pIn < pInLimit - (STEPSIZE - 1)) {
                    ulong diff = LZ4_read_ARCH64(pMatch) ^ LZ4_read_ARCH64(pIn);
                    if (diff == 0) {
                        pIn += STEPSIZE; pMatch += STEPSIZE;
                    }
                    else {
                        return LZ4_NbCommonBytes64(diff);
                    }
                }

                while (pIn < pInLimit - (STEPSIZE - 1)) {
                    ulong diff = LZ4_read_ARCH64(pMatch) ^ LZ4_read_ARCH64(pIn);
                    if (diff == 0) { pIn += STEPSIZE; pMatch += STEPSIZE; continue; }
                    pIn += LZ4_NbCommonBytes64(diff);
                    return (uint)(pIn - pStart);
                }
            }
            else {
                Debug.Assert(false);
            }

            if ((STEPSIZE == 8) && (pIn < (pInLimit - 3)) && (LZ4_read32(pMatch) == LZ4_read32(pIn))) { pIn += 4; pMatch += 4; }
            if ((pIn < (pInLimit - 1)) && (LZ4_read16(pMatch) == LZ4_read16(pIn))) { pIn += 2; pMatch += 2; }
            if ((pIn < pInLimit) && (*pMatch == *pIn)) pIn++;
            return (uint)(pIn - pStart);
        }

        private static byte[] ctz7_tab = {
                7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
                4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
        };

        private static unsafe uint LZ4_NbCommonBytes64(ulong val) {
            Debug.Assert(val != 0);
            if (BitConverter.IsLittleEndian) {
#if NET6_0_OR_GREATER
                int r = BitOperations.TrailingZeroCount(val); // _BitScanForward64
                return (uint)r >> 3;
#else
                ulong m = 0x0101010101010101UL;
                val ^= val - 1;
                return (uint)(((ulong)((val & (m - 1)) * m)) >> 56);
#endif
            }
            else   /* Big Endian CPU */
            {
                /* this method is probably faster,
				 * but adds a 128 bytes lookup table */
                ulong mask = 0x0101010101010101UL;
                ulong t = (((val >> 8) - mask) | val) & mask;
                return ctz7_tab[(t * 0x0080402010080402UL) >> 57];
            }
        }

        private static unsafe uint LZ4_NbCommonBytes32(uint val) {
            Debug.Assert(val != 0);
            if (BitConverter.IsLittleEndian) {
#if NET6_0_OR_GREATER
                int r = BitOperations.TrailingZeroCount(val); //_BitScanForward
                return (uint)r >> 3;
#else
                uint m = 0x01010101;
                return (uint)((((val - 1) ^ val) & (m - 1)) * m) >> 24;
#endif
            }
            else   /* Big Endian CPU */
            {
                val >>= 8;
                val = ((((val + 0x00FFFF00) | 0x00FFFFFF) + val) |
                    (val + 0x00FF0000)) >> 24;
                return (uint)val ^ 3;
            }
        }

        internal static int LZ4_compressBound(int isize) { return ((uint)(isize) > (uint)LZ4_MAX_INPUT_SIZE ? 0 : (isize) + ((isize) / 255) + 16); }

        private static unsafe LZ4_stream* LZ4_initStream(void* buffer, int size) {

            if (buffer == null) { return null; }
            if (size < sizeof(LZ4_stream)) { return null; }
            if (!LZ4_isAligned(buffer, LZ4_stream_t_alignment())) return null;
            System.Runtime.CompilerServices.Unsafe.InitBlock(buffer, 0, (uint)sizeof(LZ4_stream)); //MEM_INIT(buffer, 0, sizeof(LZ4_stream_t_internal));
            return (LZ4_stream*)buffer;
        }

        private static unsafe bool LZ4_isAligned(void* ptr, int alignment) {
            return ((long)ptr & (alignment - 1)) == 0;
        }

        private static int LZ4_stream_t_alignment() {
            return 1;  /* effectively disabled */
        }

        internal static unsafe int LZ4_compress_HC(byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {
            int cSize;
            LZ4_streamHC state;
            LZ4_streamHC* statePtr = &state;
            //DEBUGLOG(5, "LZ4_compress_HC")
            cSize = LZ4_compress_HC_extStateHC(statePtr, src, dst, srcSize, dstCapacity, compressionLevel);
            return cSize;
        }

        private static unsafe int LZ4_compress_HC_extStateHC(void* state, byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {
            LZ4_streamHC* ctx = LZ4_initStreamHC(state, sizeof(LZ4_streamHC));
            if (ctx == null) return 0;   /* init failure */
            return LZ4_compress_HC_extStateHC_fastReset(state, src, dst, srcSize, dstCapacity, compressionLevel);
        }

        private static unsafe LZ4_streamHC* LZ4_initStreamHC(void* buffer, int size) {
            LZ4_streamHC* LZ4_streamHCPtr = (LZ4_streamHC*)buffer;
            //DEBUGLOG(4, "LZ4_initStreamHC(%p, %u)", buffer, (uint)size);
            /* check conditions */
            if (buffer == null) return null;
            if (size < sizeof(LZ4_streamHC)) return null;
            if (!LZ4_isAligned(buffer, LZ4_streamHC_alignment())) return null;
            /* init */
            {
                LZ4HC_CCtx_internal* hcstate = &(LZ4_streamHCPtr->internal_donotuse);
                Unsafe.InitBlock(hcstate, 0, (uint)sizeof(LZ4HC_CCtx_internal));
            }
            LZ4_setCompressionLevel(LZ4_streamHCPtr, LZ4HC_CLEVEL_DEFAULT);
            return LZ4_streamHCPtr;
        }

        private static int LZ4_streamHC_alignment() {
            return 1;  /* effectively disabled */
        }

        private static unsafe void LZ4_setCompressionLevel(LZ4_streamHC* LZ4_streamHCPtr, int compressionLevel) {
            //DEBUGLOG(5, "LZ4_setCompressionLevel(%p, %d)", LZ4_streamHCPtr, compressionLevel);
            if (compressionLevel < 1) compressionLevel = LZ4HC_CLEVEL_DEFAULT;
            if (compressionLevel > LZ4HC_CLEVEL_MAX) compressionLevel = LZ4HC_CLEVEL_MAX;
            LZ4_streamHCPtr->internal_donotuse.compressionLevel = (short)compressionLevel;
        }

        private static unsafe int LZ4_compress_HC_extStateHC_fastReset(void* state, byte* src, byte* dst, int srcSize, int dstCapacity, int compressionLevel) {
            LZ4HC_CCtx_internal* ctx = &((LZ4_streamHC*)state)->internal_donotuse;
            if (!LZ4_isAligned(state, LZ4_streamHC_alignment())) return 0;
            LZ4_resetStreamHC_fast((LZ4_streamHC*)state, compressionLevel);
            LZ4HC_init_internal(ctx, (byte*)src);
            if (dstCapacity < LZ4_compressBound(srcSize))
                return LZ4HC_compress_generic(ctx, src, dst, &srcSize, dstCapacity, compressionLevel, limitedOutput_directive.limitedOutput);
            else
                return LZ4HC_compress_generic(ctx, src, dst, &srcSize, dstCapacity, compressionLevel, limitedOutput_directive.notLimited);
        }

        private static unsafe void LZ4_resetStreamHC_fast(LZ4_streamHC* LZ4_streamHCPtr, int compressionLevel) {
            LZ4HC_CCtx_internal* s = &LZ4_streamHCPtr->internal_donotuse;
            //DEBUGLOG(5, "LZ4_resetStreamHC_fast(%p, %d)", LZ4_streamHCPtr, compressionLevel);
            if (s->dirty != 0) {
                LZ4_initStreamHC(LZ4_streamHCPtr, sizeof(LZ4_streamHC));
            }
            else {
                Debug.Assert(s->end >= s->prefixStart);
                s->dictLimit += (uint)(s->end - s->prefixStart);
                s->prefixStart = null;
                s->end = null;
                s->dictCtx = null;
            }
            LZ4_setCompressionLevel(LZ4_streamHCPtr, compressionLevel);
        }

        private static unsafe void LZ4HC_init_internal(LZ4HC_CCtx_internal* hc4, byte* start) {
            ulong bufferSize = (ulong)(hc4->end - hc4->prefixStart);
            ulong newStartingOffset = bufferSize + hc4->dictLimit;
            //DEBUGLOG(5, "LZ4HC_init_internal");
            Debug.Assert(newStartingOffset >= bufferSize);  /* check overflow */
            if (newStartingOffset > 1 * GB) {
                LZ4HC_clearTables(hc4);
                newStartingOffset = 0;
            }
            newStartingOffset += 64 * KB;
            hc4->nextToUpdate = (uint)newStartingOffset;
            hc4->prefixStart = start;
            hc4->end = start;
            hc4->dictStart = start;
            hc4->dictLimit = (uint)newStartingOffset;
            hc4->lowLimit = (uint)newStartingOffset;
        }

        private static unsafe void LZ4HC_clearTables(LZ4HC_CCtx_internal* hc4) {
            Unsafe.InitBlock(hc4->hashTable, 0, lz4.LZ4HC_HASHTABLESIZE * 4);
            Unsafe.InitBlock(hc4->chainTable, 0xFF, lz4.LZ4HC_MAXD * 2);
        }

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

        private static unsafe int LZ4HC_compress_generic_dictCtx(
            LZ4HC_CCtx_internal* ctx,
            byte* src,
            byte* dst,
            int* srcSizePtr,
            int dstCapacity,
            int cLevel,
            limitedOutput_directive limit
        ) {
            ulong position = (ulong)(ctx->end - ctx->prefixStart) + (ctx->dictLimit - ctx->lowLimit);
            Debug.Assert(ctx->dictCtx != null);
            if (position >= 64 * KB) {
                ctx->dictCtx = null;
                return LZ4HC_compress_generic_noDictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
            }
            else if (position == 0 && *srcSizePtr > 4 * KB && isStateCompatible(ctx, ctx->dictCtx)) {
                Unsafe.CopyBlock(ctx, ctx->dictCtx, (uint)sizeof(LZ4HC_CCtx_internal));
                LZ4HC_setExternalDict(ctx, (byte*)src);
                ctx->compressionLevel = (short)cLevel;
                return LZ4HC_compress_generic_noDictCtx(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit);
            }
            else {
                return LZ4HC_compress_generic_internal(ctx, src, dst, srcSizePtr, dstCapacity, cLevel, limit, dictCtx_directive.usingDictCtxHc);
            }
        }

        private static unsafe bool isStateCompatible(LZ4HC_CCtx_internal* ctx1, LZ4HC_CCtx_internal* ctx2) {
            bool isMid1 = LZ4HC_getCLevelParams(ctx1->compressionLevel).strat == lz4hc_strat_e.lz4mid;
            bool isMid2 = LZ4HC_getCLevelParams(ctx2->compressionLevel).strat == lz4hc_strat_e.lz4mid;
            return !(isMid1 ^ isMid2);
        }

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
            //DEBUGLOG(5, "LZ4HC_compress_generic_internal(src=%p, srcSize=%d, dstCapacity=%d)",
            //      src, *srcSizePtr, dstCapacity);

            /* input sanitization */
            if ((uint)*srcSizePtr > (uint)LZ4_MAX_INPUT_SIZE) return 0;  /* Unsupported input size (too large or negative) */
            if (dstCapacity < 1) return 0;   /* Invalid: impossible to store anything */
            Debug.Assert(dst != null); /* since dstCapacity >= 1, dst must be valid */
            if (*srcSizePtr == 0) { *dst = 0; return 1; }
            Debug.Assert(src != null); /* since *srcSizePtr >= 1, src must be valid */

            ctx->end += *srcSizePtr;
            {
                cParams_t cParam = LZ4HC_getCLevelParams(cLevel);
                HCfavor_e favor = ctx->favorDecSpeed != 0 ? HCfavor_e.favorDecompressionSpeed : HCfavor_e.favorCompressionRatio;
                int result;

                if (cParam.strat == lz4hc_strat_e.lz4mid) {
                    if (IntPtr.Size == 8) {
                        result = LZ4MID_compress64(ctx,
                                            src, dst, srcSizePtr, dstCapacity,
                                            limit, dict);
                    }
                    else {
                        result = LZ4MID_compress32(ctx,
                                            src, dst, srcSizePtr, dstCapacity,
                                            limit, dict);
                    }
                }
                else if (cParam.strat == lz4hc_strat_e.lz4hc) {
                    if (IntPtr.Size == 8) {
                        result = LZ4HC_compress_hashChain64(ctx,
                                        src, dst, srcSizePtr, dstCapacity,
                                        cParam.nbSearches, limit, dict);
                    }
                    else {
                        result = LZ4HC_compress_hashChain32(ctx,
                                      src, dst, srcSizePtr, dstCapacity,
                                      cParam.nbSearches, limit, dict);
                    }
                }
                else {
                    Debug.Assert(cParam.strat == lz4hc_strat_e.lz4opt);
                    if (IntPtr.Size == 8) {
                        result = LZ4HC_compress_optimal64(ctx,
                                        src, dst, srcSizePtr, dstCapacity,
                                        cParam.nbSearches, cParam.targetLength, limit,
                                        cLevel >= LZ4HC_CLEVEL_MAX,   /* ultra mode */
                                        dict, favor);
                    }
                    else {
                        result = LZ4HC_compress_optimal32(ctx,
                                        src, dst, srcSizePtr, dstCapacity,
                                        cParam.nbSearches, cParam.targetLength, limit,
                                        cLevel >= LZ4HC_CLEVEL_MAX,   /* ultra mode */
                                        dict, favor);
                    }
                }
                if (result <= 0) ctx->dirty = 1;
                return result;
            }
        }

        private static cParams_t LZ4HC_getCLevelParams(int cLevel) {
            /* note : clevel convention is a bit different from lz4frame,
        * possibly something worth revisiting for consistency */
            if (cLevel < 1)
                cLevel = LZ4HC_CLEVEL_DEFAULT;
            cLevel = Math.Min(LZ4HC_CLEVEL_MAX, cLevel);
            return k_clTable[cLevel];
        }

        private static unsafe void LZ4HC_setExternalDict(LZ4HC_CCtx_internal* ctxPtr, byte* newBlock) {
            //DEBUGLOG(4, "LZ4HC_setExternalDict(%p, %p)", ctxPtr, newBlock);
            if ((ctxPtr->end >= ctxPtr->prefixStart + 4)
              && (LZ4HC_getCLevelParams(ctxPtr->compressionLevel).strat != lz4hc_strat_e.lz4mid)) {
                LZ4HC_Insert(ctxPtr, ctxPtr->end - 3);  /* Referencing remaining dictionary content */
            }

            /* Only one memory segment for extDict, so any previous extDict is lost at this stage */
            ctxPtr->lowLimit = ctxPtr->dictLimit;
            ctxPtr->dictStart = ctxPtr->prefixStart;
            ctxPtr->dictLimit += (uint)(ctxPtr->end - ctxPtr->prefixStart);
            ctxPtr->prefixStart = newBlock;
            ctxPtr->end = newBlock;
            ctxPtr->nextToUpdate = ctxPtr->dictLimit;   /* match referencing will resume from there */

            /* cannot reference an extDict and a dictCtx at the same time */
            ctxPtr->dictCtx = null;
        }

        private static unsafe void LZ4HC_Insert(LZ4HC_CCtx_internal* hc4, byte* ip) {
            ushort* chainTable = hc4->chainTable;
            uint* hashTable = hc4->hashTable;
            byte* prefixPtr = hc4->prefixStart;
            uint prefixIdx = hc4->dictLimit;
            uint target = (uint)(ip - prefixPtr) + prefixIdx;
            uint idx = hc4->nextToUpdate;
            Debug.Assert(ip >= prefixPtr);
            Debug.Assert(target >= prefixIdx);

            while (idx < target) {
                uint h = LZ4HC_hashPtr(prefixPtr + idx - prefixIdx);
                uint delta = idx - hashTable[h];
                if (delta > LZ4_DISTANCE_MAX) delta = LZ4_DISTANCE_MAX;
                chainTable[(ushort)idx] = (ushort)delta;
                hashTable[h] = idx;
                idx++;
            }

            hc4->nextToUpdate = target;
        }

        private static unsafe uint HASH_FUNCTION(uint i) => (((i) * 2654435761U) >> ((MINMATCH * 8) - LZ4HC_HASH_LOG));

        private static unsafe uint LZ4HC_hashPtr(void* ptr) { return HASH_FUNCTION(LZ4_read32(ptr)); }

        private static unsafe int LZ4MID_compress64(
            LZ4HC_CCtx_internal* ctx,
            byte* src,
            byte* dst,
            int* srcSizePtr,
            int maxOutputSize,
            limitedOutput_directive limit,
            dictCtx_directive dict
        ) {
            uint* hash4Table = ctx->hashTable;
            uint* hash8Table = hash4Table + LZ4MID_HASHTABLESIZE;
            byte* ip = (byte*)src;
            byte* anchor = ip;
            byte* iend = ip + *srcSizePtr;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = (iend - LASTLITERALS);
            byte* ilimit = (iend - LZ4MID_HASHSIZE);
            byte* op = (byte*)dst;
            byte* oend = op + maxOutputSize;

            byte* prefixPtr = ctx->prefixStart;
            uint prefixIdx = ctx->dictLimit;
            uint ilimitIdx = (uint)(ilimit - prefixPtr) + prefixIdx;
            byte* dictStart = ctx->dictStart;
            uint dictIdx = ctx->lowLimit;
            uint gDictEndIndex = ctx->lowLimit;
            LZ4MID_searchIntoDict_f? searchIntoDict = (dict == dictCtx_directive.usingDictCtxHc) ? select_searchDict_function(ctx->dictCtx) : null;
            uint matchLength;
            uint matchDistance;

            unsafe void ADDPOS4(byte* _p, uint _idx) { LZ4MID_addPosition(hash4Table, LZ4MID_hash4Ptr(_p), _idx); }

            unsafe void ADDPOS8(byte* _p, uint _idx) { LZ4MID_addPosition(hash8Table, LZ4MID_hash8Ptr(_p), _idx); }

            //DEBUGLOG(5, "LZ4MID_compress (%i bytes)", *srcSizePtr);

            /* preconditions verifications */
            //if (dict == dictCtx_directive.usingDictCtxHc) DEBUGLOG(5, "usingDictCtxHc");
            Debug.Assert(*srcSizePtr > 0);
            Debug.Assert(*srcSizePtr <= LZ4_MAX_INPUT_SIZE);
            Debug.Assert(src != null);
            Debug.Assert(maxOutputSize >= 1);
            Debug.Assert(dst != null);

            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;  /* Hack for support LZ4 format restriction */
            if (*srcSizePtr < LZ4_minLength)
                goto _lz4mid_last_literals;  /* Input too small, no compression (all literals) */

            /* main loop */
            while (ip <= mflimit) {
                uint ipIndex = (uint)(ip - prefixPtr) + prefixIdx;
                /* search long match */
                {
                    uint h8 = LZ4MID_hash8Ptr(ip);
                    uint pos8 = hash8Table[h8];
                    Debug.Assert(h8 < LZ4MID_HASHTABLESIZE);
                    Debug.Assert(pos8 < ipIndex);
                    LZ4MID_addPosition(hash8Table, h8, ipIndex);
                    if (ipIndex - pos8 <= LZ4_DISTANCE_MAX) {
                        /* match candidate found */
                        if (pos8 >= prefixIdx) {
                            byte* matchPtr = prefixPtr + pos8 - prefixIdx;
                            Debug.Assert(matchPtr < ip);
                            matchLength = LZ4_count(ip, matchPtr, matchlimit);
                            if (matchLength >= MINMATCH) {
                                //DEBUGLOG(7, "found long match at pos %u (len=%u)", pos8, matchLength);
                                matchDistance = ipIndex - pos8;
                                goto _lz4mid_encode_sequence;
                            }
                        }
                        else {
                            if (pos8 >= dictIdx) {
                                /* extDict match candidate */
                                byte* matchPtr = dictStart + (pos8 - dictIdx);
                                ulong safeLen = Math.Min(prefixIdx - pos8, (ulong)(matchlimit - ip));
                                matchLength = LZ4_count(ip, matchPtr, ip + safeLen);
                                if (matchLength >= MINMATCH) {
                                    //DEBUGLOG(7, "found long match at ExtDict pos %u (len=%u)", pos8, matchLength);
                                    matchDistance = ipIndex - pos8;
                                    goto _lz4mid_encode_sequence;
                                }
                            }
                        }
                    }
                }
                /* search short match */
                {
                    uint h4 = LZ4MID_hash4Ptr(ip);
                    uint pos4 = hash4Table[h4];
                    Debug.Assert(h4 < LZ4MID_HASHTABLESIZE);
                    Debug.Assert(pos4 < ipIndex);
                    LZ4MID_addPosition(hash4Table, h4, ipIndex);
                    if (ipIndex - pos4 <= LZ4_DISTANCE_MAX) {
                        /* match candidate found */
                        if (pos4 >= prefixIdx) {
                            /* only search within prefix */
                            byte* matchPtr = prefixPtr + (pos4 - prefixIdx);
                            Debug.Assert(matchPtr < ip);
                            Debug.Assert(matchPtr >= prefixPtr);
                            matchLength = LZ4_count(ip, matchPtr, matchlimit);
                            if (matchLength >= MINMATCH) {
                                /* short match found, let's just check ip+1 for longer */
                                uint h8 = LZ4MID_hash8Ptr(ip + 1);
                                uint pos8 = hash8Table[h8];
                                uint m2Distance = ipIndex + 1 - pos8;
                                matchDistance = ipIndex - pos4;
                                if (m2Distance <= LZ4_DISTANCE_MAX
                                && pos8 >= prefixIdx /* only search within prefix */
                                && (ip < mflimit)
                                ) {
                                    byte* m2Ptr = prefixPtr + (pos8 - prefixIdx);
                                    uint ml2 = LZ4_count(ip + 1, m2Ptr, matchlimit);
                                    if (ml2 > matchLength) {
                                        LZ4MID_addPosition(hash8Table, h8, ipIndex + 1);
                                        ip++;
                                        matchLength = ml2;
                                        matchDistance = m2Distance;
                                    }
                                }
                                goto _lz4mid_encode_sequence;
                            }
                        }
                        else {
                            if (pos4 >= dictIdx) {
                                /* extDict match candidate */
                                byte* matchPtr = dictStart + (pos4 - dictIdx);
                                ulong safeLen = Math.Min(prefixIdx - pos4, (ulong)(matchlimit - ip));
                                matchLength = LZ4_count(ip, matchPtr, ip + safeLen);
                                if (matchLength >= MINMATCH) {
                                    //DEBUGLOG(7, "found match at ExtDict pos %u (len=%u)", pos4, matchLength);
                                    matchDistance = ipIndex - pos4;
                                    goto _lz4mid_encode_sequence;
                                }
                            }
                        }
                    }
                }
                /* no match found in prefix */
                if ((dict == dictCtx_directive.usingDictCtxHc)
                  && (ipIndex - gDictEndIndex < LZ4_DISTANCE_MAX - 8)) {
                    /* search a match into external dictionary */
                    LZ4HC_match_t dMatch = searchIntoDict(ip, ipIndex,
                            matchlimit,
                            ctx->dictCtx, gDictEndIndex);
                    if (dMatch.len >= MINMATCH) {
                        //DEBUGLOG(7, "found Dictionary match (offset=%i)", dMatch.off);
                        Debug.Assert(dMatch.back == 0);
                        matchLength = (uint)dMatch.len;
                        matchDistance = (uint)dMatch.off;
                        goto _lz4mid_encode_sequence;
                    }
                }
                /* no match found */
                ip += 1 + ((ip - anchor) >> 9);  /* skip faster over incompressible data */
                continue;

            _lz4mid_encode_sequence:
                /* catch back */
                while (((ip > anchor) & ((uint)(ip - prefixPtr) > matchDistance)) && (ip[-1] == ip[-(int)matchDistance - 1])) {
                    ip--; matchLength++;
                }
                ;

                /* fill table with beginning of match */
                ADDPOS8(ip + 1, ipIndex + 1);
                ADDPOS8(ip + 2, ipIndex + 2);
                ADDPOS4(ip + 1, ipIndex + 1);

                /* encode */
                {
                    byte* saved_op = op;
                    /* LZ4HC_encodeSequence always updates @op; on success, it updates @ip and @anchor */
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            (int)matchLength, (int)matchDistance,
                            limit, oend)) {
                        op = saved_op;  /* restore @op value before failed LZ4HC_encodeSequence */
                        goto _lz4mid_dest_overflow;
                    }
                }

                /* fill table with end of match */
                {
                    uint endMatchIdx = (uint)(ip - prefixPtr) + prefixIdx;
                    uint pos_m2 = endMatchIdx - 2;
                    if (pos_m2 < ilimitIdx) {
                        if (ip - prefixPtr > 5) {
                            ADDPOS8(ip - 5, endMatchIdx - 5);
                        }
                        ADDPOS8(ip - 3, endMatchIdx - 3);
                        ADDPOS8(ip - 2, endMatchIdx - 2);
                        ADDPOS4(ip - 2, endMatchIdx - 2);
                        ADDPOS4(ip - 1, endMatchIdx - 1);
                    }
                }
            }

        _lz4mid_last_literals:
            /* Encode Last Literals */
            {
                ulong lastRunSize = (ulong)(iend - anchor);  /* literals */
                ulong llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                ulong totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) return 0;  /* not enough space in @dst */
                    /* adapt lastRunSize to fill 'dest' */
                    lastRunSize = (ulong)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize;  /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    ulong accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255)
                        *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Debug.Assert(lastRunSize <= (ulong)(oend - op));
                Buffer.MemoryCopy(anchor, op, lastRunSize, lastRunSize); // LZ4_memcpy(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            //DEBUGLOG(5, "compressed %i bytes into %i bytes", *srcSizePtr, (int)((byte*)op - dst));
            Debug.Assert(ip >= (byte*)src);
            Debug.Assert(ip <= iend);
            *srcSizePtr = (int)(ip - (byte*)src);
            Debug.Assert((byte*)op >= dst);
            Debug.Assert(op <= oend);
            Debug.Assert((byte*)op - dst < long.MaxValue);
            return (int)((byte*)op - dst);

        _lz4mid_dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : @ip, @anchor, @optr and @matchLength must be set correctly */
                ulong ll = (ulong)(ip - anchor);
                ulong ll_addbytes = (ll + 240) / 255;
                ulong ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence is overflowing : %u literals, %u remaining space",
                //        (uint)ll, (uint)(oend - op));
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    ulong bytesLeftForMl = (ulong)(maxLitPos - (op + ll_totalCost));
                    ulong maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < long.MaxValue);
                    if ((ulong)matchLength > maxMlSize) matchLength = (uint)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + matchLength >= MFLIMIT) {
                        //DEBUGLOG(6, "Let's encode a last sequence (ll=%u, ml=%u)", (uint)ll, matchLength);
                        LZ4HC_encodeSequence(&ip, &op, &anchor,
                                (int)matchLength, (int)matchDistance,
                                limitedOutput_directive.notLimited, oend);
                    }
                }
                //DEBUGLOG(6, "Let's finish with a run of literals (%u bytes left)", (uint)(oend - op));
                goto _lz4mid_last_literals;
            }
            /* compression failed */
            return 0;
        }

        private static unsafe int LZ4MID_compress32(
           LZ4HC_CCtx_internal* ctx,
           byte* src,
           byte* dst,
           int* srcSizePtr,
           int maxOutputSize,
           limitedOutput_directive limit,
           dictCtx_directive dict
       ) {
            uint* hash4Table = ctx->hashTable;
            uint* hash8Table = hash4Table + LZ4MID_HASHTABLESIZE;
            byte* ip = (byte*)src;
            byte* anchor = ip;
            byte* iend = ip + *srcSizePtr;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = (iend - LASTLITERALS);
            byte* ilimit = (iend - LZ4MID_HASHSIZE);
            byte* op = (byte*)dst;
            byte* oend = op + maxOutputSize;

            byte* prefixPtr = ctx->prefixStart;
            uint prefixIdx = ctx->dictLimit;
            uint ilimitIdx = (uint)(ilimit - prefixPtr) + prefixIdx;
            byte* dictStart = ctx->dictStart;
            uint dictIdx = ctx->lowLimit;
            uint gDictEndIndex = ctx->lowLimit;
            LZ4MID_searchIntoDict_f? searchIntoDict = (dict == dictCtx_directive.usingDictCtxHc) ? select_searchDict_function(ctx->dictCtx) : null;
            uint matchLength;
            uint matchDistance;

            unsafe void ADDPOS4(byte* _p, uint _idx) { LZ4MID_addPosition(hash4Table, LZ4MID_hash4Ptr(_p), _idx); }

            unsafe void ADDPOS8(byte* _p, uint _idx) { LZ4MID_addPosition(hash8Table, LZ4MID_hash8Ptr(_p), _idx); }

            //DEBUGLOG(5, "LZ4MID_compress (%i bytes)", *srcSizePtr);

            /* preconditions verifications */
            //if (dict == dictCtx_directive.usingDictCtxHc) DEBUGLOG(5, "usingDictCtxHc");
            Debug.Assert(*srcSizePtr > 0);
            Debug.Assert(*srcSizePtr <= LZ4_MAX_INPUT_SIZE);
            Debug.Assert(src != null);
            Debug.Assert(maxOutputSize >= 1);
            Debug.Assert(dst != null);

            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;  /* Hack for support LZ4 format restriction */
            if (*srcSizePtr < LZ4_minLength)
                goto _lz4mid_last_literals;  /* Input too small, no compression (all literals) */

            /* main loop */
            while (ip <= mflimit) {
                uint ipIndex = (uint)(ip - prefixPtr) + prefixIdx;
                /* search long match */
                {
                    uint h8 = LZ4MID_hash8Ptr(ip);
                    uint pos8 = hash8Table[h8];
                    Debug.Assert(h8 < LZ4MID_HASHTABLESIZE);
                    Debug.Assert(pos8 < ipIndex);
                    LZ4MID_addPosition(hash8Table, h8, ipIndex);
                    if (ipIndex - pos8 <= LZ4_DISTANCE_MAX) {
                        /* match candidate found */
                        if (pos8 >= prefixIdx) {
                            byte* matchPtr = prefixPtr + pos8 - prefixIdx;
                            Debug.Assert(matchPtr < ip);
                            matchLength = LZ4_count(ip, matchPtr, matchlimit);
                            if (matchLength >= MINMATCH) {
                                //DEBUGLOG(7, "found long match at pos %u (len=%u)", pos8, matchLength);
                                matchDistance = ipIndex - pos8;
                                goto _lz4mid_encode_sequence;
                            }
                        }
                        else {
                            if (pos8 >= dictIdx) {
                                /* extDict match candidate */
                                byte* matchPtr = dictStart + (pos8 - dictIdx);
                                uint safeLen = Math.Min(prefixIdx - pos8, (uint)(matchlimit - ip));
                                matchLength = LZ4_count(ip, matchPtr, ip + safeLen);
                                if (matchLength >= MINMATCH) {
                                    //DEBUGLOG(7, "found long match at ExtDict pos %u (len=%u)", pos8, matchLength);
                                    matchDistance = ipIndex - pos8;
                                    goto _lz4mid_encode_sequence;
                                }
                            }
                        }
                    }
                }
                /* search short match */
                {
                    uint h4 = LZ4MID_hash4Ptr(ip);
                    uint pos4 = hash4Table[h4];
                    Debug.Assert(h4 < LZ4MID_HASHTABLESIZE);
                    Debug.Assert(pos4 < ipIndex);
                    LZ4MID_addPosition(hash4Table, h4, ipIndex);
                    if (ipIndex - pos4 <= LZ4_DISTANCE_MAX) {
                        /* match candidate found */
                        if (pos4 >= prefixIdx) {
                            /* only search within prefix */
                            byte* matchPtr = prefixPtr + (pos4 - prefixIdx);
                            Debug.Assert(matchPtr < ip);
                            Debug.Assert(matchPtr >= prefixPtr);
                            matchLength = LZ4_count(ip, matchPtr, matchlimit);
                            if (matchLength >= MINMATCH) {
                                /* short match found, let's just check ip+1 for longer */
                                uint h8 = LZ4MID_hash8Ptr(ip + 1);
                                uint pos8 = hash8Table[h8];
                                uint m2Distance = ipIndex + 1 - pos8;
                                matchDistance = ipIndex - pos4;
                                if (m2Distance <= LZ4_DISTANCE_MAX
                                && pos8 >= prefixIdx /* only search within prefix */
                                && (ip < mflimit)
                                ) {
                                    byte* m2Ptr = prefixPtr + (pos8 - prefixIdx);
                                    uint ml2 = LZ4_count(ip + 1, m2Ptr, matchlimit);
                                    if (ml2 > matchLength) {
                                        LZ4MID_addPosition(hash8Table, h8, ipIndex + 1);
                                        ip++;
                                        matchLength = ml2;
                                        matchDistance = m2Distance;
                                    }
                                }
                                goto _lz4mid_encode_sequence;
                            }
                        }
                        else {
                            if (pos4 >= dictIdx) {
                                /* extDict match candidate */
                                byte* matchPtr = dictStart + (pos4 - dictIdx);
                                uint safeLen = Math.Min(prefixIdx - pos4, (uint)(matchlimit - ip));
                                matchLength = LZ4_count(ip, matchPtr, ip + safeLen);
                                if (matchLength >= MINMATCH) {
                                    //DEBUGLOG(7, "found match at ExtDict pos %u (len=%u)", pos4, matchLength);
                                    matchDistance = ipIndex - pos4;
                                    goto _lz4mid_encode_sequence;
                                }
                            }
                        }
                    }
                }
                /* no match found in prefix */
                if ((dict == dictCtx_directive.usingDictCtxHc)
                  && (ipIndex - gDictEndIndex < LZ4_DISTANCE_MAX - 8)) {
                    /* search a match into external dictionary */
                    LZ4HC_match_t dMatch = searchIntoDict(ip, ipIndex,
                            matchlimit,
                            ctx->dictCtx, gDictEndIndex);
                    if (dMatch.len >= MINMATCH) {
                        //DEBUGLOG(7, "found Dictionary match (offset=%i)", dMatch.off);
                        Debug.Assert(dMatch.back == 0);
                        matchLength = (uint)dMatch.len;
                        matchDistance = (uint)dMatch.off;
                        goto _lz4mid_encode_sequence;
                    }
                }
                /* no match found */
                ip += 1 + ((ip - anchor) >> 9);  /* skip faster over incompressible data */
                continue;

            _lz4mid_encode_sequence:
                /* catch back */
                while (((ip > anchor) & ((uint)(ip - prefixPtr) > matchDistance)) && (ip[-1] == ip[-(int)matchDistance - 1])) {
                    ip--; matchLength++;
                }
                ;

                /* fill table with beginning of match */
                ADDPOS8(ip + 1, ipIndex + 1);
                ADDPOS8(ip + 2, ipIndex + 2);
                ADDPOS4(ip + 1, ipIndex + 1);

                /* encode */
                {
                    byte* saved_op = op;
                    /* LZ4HC_encodeSequence always updates @op; on success, it updates @ip and @anchor */
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            (int)matchLength, (int)matchDistance,
                            limit, oend)) {
                        op = saved_op;  /* restore @op value before failed LZ4HC_encodeSequence */
                        goto _lz4mid_dest_overflow;
                    }
                }

                /* fill table with end of match */
                {
                    uint endMatchIdx = (uint)(ip - prefixPtr) + prefixIdx;
                    uint pos_m2 = endMatchIdx - 2;
                    if (pos_m2 < ilimitIdx) {
                        if (ip - prefixPtr > 5) {
                            ADDPOS8(ip - 5, endMatchIdx - 5);
                        }
                        ADDPOS8(ip - 3, endMatchIdx - 3);
                        ADDPOS8(ip - 2, endMatchIdx - 2);
                        ADDPOS4(ip - 2, endMatchIdx - 2);
                        ADDPOS4(ip - 1, endMatchIdx - 1);
                    }
                }
            }

        _lz4mid_last_literals:
            /* Encode Last Literals */
            {
                uint lastRunSize = (uint)(iend - anchor);  /* literals */
                uint llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                uint totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) return 0;  /* not enough space in @dst */
                    /* adapt lastRunSize to fill 'dest' */
                    lastRunSize = (uint)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize;  /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    uint accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255)
                        *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Debug.Assert(lastRunSize <= (uint)(oend - op));
                Unsafe.CopyBlock(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            //DEBUGLOG(5, "compressed %i bytes into %i bytes", *srcSizePtr, (int)((byte*)op - dst));
            Debug.Assert(ip >= (byte*)src);
            Debug.Assert(ip <= iend);
            *srcSizePtr = (int)(ip - (byte*)src);
            Debug.Assert((byte*)op >= dst);
            Debug.Assert(op <= oend);
            Debug.Assert((byte*)op - dst < int.MaxValue);
            return (int)((byte*)op - dst);

        _lz4mid_dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : @ip, @anchor, @optr and @matchLength must be set correctly */
                uint ll = (uint)(ip - anchor);
                uint ll_addbytes = (ll + 240) / 255;
                uint ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence is overflowing : %u literals, %u remaining space",
                //        (uint)ll, (uint)(oend - op));
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    uint bytesLeftForMl = (uint)(maxLitPos - (op + ll_totalCost));
                    uint maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < int.MaxValue);
                    if ((uint)matchLength > maxMlSize) matchLength = (uint)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + matchLength >= MFLIMIT) {
                        //DEBUGLOG(6, "Let's encode a last sequence (ll=%u, ml=%u)", (uint)ll, matchLength);
                        LZ4HC_encodeSequence(&ip, &op, &anchor,
                                (int)matchLength, (int)matchDistance,
                                limitedOutput_directive.notLimited, oend);
                    }
                }
                //DEBUGLOG(6, "Let's finish with a run of literals (%u bytes left)", (uint)(oend - op));
                goto _lz4mid_last_literals;
            }
            /* compression failed */
            return 0;
        }

        private static uint LZ4MID_hash7(ulong v) { return (uint)(((v << (64 - 56)) * 58295818150454627UL) >> (64 - LZ4MID_HASHLOG)); }

        private static unsafe uint LZ4MID_hash8Ptr(void* ptr) { return LZ4MID_hash7(LZ4_readLE64(ptr)); }

        private static unsafe ulong LZ4_readLE64(void* memPtr) {
            if (BitConverter.IsLittleEndian) {
                return LZ4_read64(memPtr);
            }
            else {
                byte* p = (byte*)memPtr;
                /* note: relies on the compiler to simplify this expression */
                return (ulong)p[0] | ((ulong)p[1] << 8) | ((ulong)p[2] << 16) | ((ulong)p[3] << 24)
                    | ((ulong)p[4] << 32) | ((ulong)p[5] << 40) | ((ulong)p[6] << 48) | ((ulong)p[7] << 56);
            }
        }

        private static unsafe ulong LZ4_read64(void* memPtr) {
            ulong val; Unsafe.CopyBlock(&val, memPtr, 8); return val;
        }

        private static unsafe void LZ4MID_addPosition(uint* hTable, uint hValue, uint index) {
            hTable[hValue] = index;
        }

        private static uint LZ4MID_hash4(uint v) { return (v * 2654435761U) >> (32 - LZ4MID_HASHLOG); }

        private static unsafe uint LZ4MID_hash4Ptr(void* ptr) {
            return LZ4MID_hash4(LZ4_read32(ptr));
        }

        private unsafe delegate LZ4HC_match_t LZ4MID_searchIntoDict_f(byte* ip, uint ipIndex,
                    byte* iHighLimit,
                    LZ4HC_CCtx_internal* dictCtx, uint gDictEndIndex);

        private static unsafe LZ4MID_searchIntoDict_f? select_searchDict_function(LZ4HC_CCtx_internal* dictCtx) {
            if (dictCtx == null) return null;
            if (LZ4HC_getCLevelParams(dictCtx->compressionLevel).strat == lz4hc_strat_e.lz4mid)
                return LZ4MID_searchExtDict;
            return LZ4MID_searchHCDict;
        }

        private static unsafe LZ4HC_match_t LZ4MID_searchExtDict(byte* ip, uint ipIndex,
            byte* iHighLimit,
            LZ4HC_CCtx_internal* dictCtx, uint gDictEndIndex) {

            ulong lDictEndIndex = (ulong)(dictCtx->end - dictCtx->prefixStart) + dictCtx->dictLimit;
            uint* hash4Table = dictCtx->hashTable;
            uint* hash8Table = hash4Table + LZ4MID_HASHTABLESIZE;
            //DEBUGLOG(7, "LZ4MID_searchExtDict (ipIdx=%u)", ipIndex);

            /* search long match first */
            {
                uint l8DictMatchIndex = hash8Table[LZ4MID_hash8Ptr(ip)];
                uint m8Index = l8DictMatchIndex + gDictEndIndex - (uint)lDictEndIndex;
                Debug.Assert(lDictEndIndex <= 1 * GB);
                if (ipIndex - m8Index <= LZ4_DISTANCE_MAX) {
                    byte* matchPtr = dictCtx->prefixStart - dictCtx->dictLimit + l8DictMatchIndex;
                    ulong safeLen = Math.Min(lDictEndIndex - l8DictMatchIndex, (ulong)(iHighLimit - ip));
                    int mlt = (int)LZ4_count(ip, matchPtr, ip + safeLen);
                    if (mlt >= MINMATCH) {
                        LZ4HC_match_t md;
                        //DEBUGLOG(7, "Found long ExtDict match of len=%u", mlt);
                        md.len = mlt;
                        md.off = (int)(ipIndex - m8Index);
                        md.back = 0;
                        return md;
                    }
                }
            }

            /* search for short match second */
            {
                uint l4DictMatchIndex = hash4Table[LZ4MID_hash4Ptr(ip)];
                uint m4Index = l4DictMatchIndex + gDictEndIndex - (uint)lDictEndIndex;
                if (ipIndex - m4Index <= LZ4_DISTANCE_MAX) {
                    byte* matchPtr = dictCtx->prefixStart - dictCtx->dictLimit + l4DictMatchIndex;
                    ulong safeLen = Math.Min(lDictEndIndex - l4DictMatchIndex, (ulong)(iHighLimit - ip));
                    int mlt = (int)LZ4_count(ip, matchPtr, ip + safeLen);
                    if (mlt >= MINMATCH) {
                        LZ4HC_match_t md;
                        //DEBUGLOG(7, "Found short ExtDict match of len=%u", mlt);
                        md.len = mlt;
                        md.off = (int)(ipIndex - m4Index);
                        md.back = 0;
                        return md;
                    }
                }
            }

            /* nothing found */
            {
                LZ4HC_match_t md = new() { off = 0, len = 0, back = 0 };
                return md;
            }
        }

        private static unsafe LZ4HC_match_t LZ4MID_searchHCDict(byte* ip, uint ipIndex,
            byte* iHighLimit,
            LZ4HC_CCtx_internal* dictCtx, uint gDictEndIndex) {

            return LZ4HC_searchExtDict(ip, ipIndex,
                            ip, iHighLimit,
                            dictCtx, gDictEndIndex,
                            MINMATCH - 1, 2);
        }

        private static unsafe LZ4HC_match_t LZ4HC_searchExtDict(byte* ip, uint ipIndex,
            byte* iLowLimit, byte* iHighLimit,
            LZ4HC_CCtx_internal* dictCtx, uint gDictEndIndex,
            int currentBestML, int nbAttempts) {

            ulong lDictEndIndex = (ulong)(dictCtx->end - dictCtx->prefixStart) + dictCtx->dictLimit;
            uint lDictMatchIndex = dictCtx->hashTable[LZ4HC_hashPtr(ip)];
            uint matchIndex = lDictMatchIndex + gDictEndIndex - (uint)lDictEndIndex;
            int offset = 0, sBack = 0;
            Debug.Assert(lDictEndIndex <= 1 * GB);
            //if (lDictMatchIndex > 0)
            //    DEBUGLOG(7, "lDictEndIndex = %zu, lDictMatchIndex = %u", lDictEndIndex, lDictMatchIndex);
            while (ipIndex - matchIndex <= LZ4_DISTANCE_MAX && nbAttempts-- != 0) {
                byte* matchPtr = dictCtx->prefixStart - dictCtx->dictLimit + lDictMatchIndex;

                if (LZ4_read32(matchPtr) == LZ4_read32(ip)) {
                    int mlt;
                    int back = 0;
                    byte* vLimit = ip + (lDictEndIndex - lDictMatchIndex);
                    if (vLimit > iHighLimit) vLimit = iHighLimit;
                    mlt = (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, vLimit) + MINMATCH;
                    back = (ip > iLowLimit) ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictCtx->prefixStart) : 0;
                    mlt -= back;
                    if (mlt > currentBestML) {
                        currentBestML = mlt;
                        offset = (int)(ipIndex - matchIndex);
                        sBack = back;
                        //DEBUGLOG(7, "found match of length %i within extDictCtx", currentBestML);
                    }
                }

                {
                    uint nextOffset = DELTANEXTU16(dictCtx->chainTable, lDictMatchIndex);
                    lDictMatchIndex -= nextOffset;
                    matchIndex -= nextOffset;
                }
            }

            {
                LZ4HC_match_t md;
                md.len = currentBestML;
                md.off = offset;
                md.back = sBack;
                return md;
            }
        }

        private static unsafe bool LZ4HC_encodeSequence(
            byte** _ip,
            byte** _op,
            byte** _anchor,
            int matchLength,
            int offset,
            limitedOutput_directive limit,
            byte* oend) {

            byte* token = (*_op)++;

            /* Encode Literal length */
            {
                ulong litLen = (ulong)((*_ip) - (*_anchor));
                //LZ4_STATIC_ASSERT(notLimited == 0);
                /* Check output limit */
                if (limit != limitedOutput_directive.notLimited && (((*_op) + (litLen / 255) + litLen + (2 + 1 + LASTLITERALS)) > oend)) {
                    //DEBUGLOG(6, "Not enough room to write %i literals (%i bytes remaining)",
                    //        (int)litLen, (int)(oend - (*_op)));
                    return true;
                }
                if (litLen >= RUN_MASK) {
                    ulong len = litLen - RUN_MASK;
                    *token = (RUN_MASK << ML_BITS);
                    for (; len >= 255; len -= 255) *(*_op)++ = 255;
                    *(*_op)++ = (byte)len;
                }
                else {
                    *token = (byte)(litLen << ML_BITS);
                }

                /* Copy Literals */
                LZ4_wildCopy8((*_op), (*_anchor), (*_op) + litLen);
                (*_op) += litLen;
            }

            /* Encode Offset */
            Debug.Assert(offset <= LZ4_DISTANCE_MAX);
            Debug.Assert(offset > 0);
            LZ4_writeLE16((*_op), (ushort)(offset)); (*_op) += 2;

            /* Encode MatchLength */
            Debug.Assert(matchLength >= MINMATCH);
            {
                ulong mlCode = (ulong)matchLength - MINMATCH;
                if (limit != limitedOutput_directive.notLimited && ((*_op) + (mlCode / 255) + (1 + LASTLITERALS) > oend)) {
                    //DEBUGLOG(6, "Not enough room to write match length");
                    return true;   /* Check output limit */
                }
                if (mlCode >= ML_MASK) {
                    *token += ML_MASK;
                    mlCode -= ML_MASK;
                    for (; mlCode >= 510; mlCode -= 510) { *(*_op)++ = 255; *(*_op)++ = 255; }
                    if (mlCode >= 255) { mlCode -= 255; *(*_op)++ = 255; }
                    *(*_op)++ = (byte)mlCode;
                }
                else {
                    *token += (byte)(mlCode);
                }
            }

            /* Prepare next loop */
            (*_ip) += matchLength;
            (*_anchor) = (*_ip);

            return false;
        }

        private static unsafe ushort DELTANEXTU16(ushort* table, uint pos) => table[(ushort)(pos)];

        private static unsafe int LZ4HC_countBack(byte* ip, byte* match,
            byte* iMin, byte* mMin) {

            int back = 0;
            int min = (int)Math.Max(iMin - ip, mMin - match);
            Debug.Assert(min <= 0);
            Debug.Assert(ip >= iMin); Debug.Assert((ulong)(ip - iMin) < (1U << 31));
            Debug.Assert(match >= mMin); Debug.Assert((ulong)(match - mMin) < (1U << 31));

            while ((back - min) > 3) {
                uint v = LZ4_read32(ip + back - 4) ^ LZ4_read32(match + back - 4);
                if (v != 0) {
                    return (back - (int)LZ4HC_NbCommonBytes32(v));
                }
                else back -= 4; /* 4-byte step */
            }
            /* check remainder if any */
            while ((back > min)
                 && (ip[back - 1] == match[back - 1]))
                back--;
            return back;
        }

        private static uint LZ4HC_NbCommonBytes32(uint val) {
            Debug.Assert(val != 0);
            if (BitConverter.IsLittleEndian) {
#if NET6_0_OR_GREATER
                int r = BitOperations.Log2(val); // _BitScanReverse
                return (uint)((31 - r) >> 3);
#else
                val >>= 8;
                val = ((((val + 0x00FFFF00) | 0x00FFFFFF) + val) |
                    (val + 0x00FF0000)) >> 24;
                return (uint)val ^ 3;
#endif
            }
            else {
                const uint m = 0x01010101;
                return (uint)((((val - 1) ^ val) & (m - 1)) * m) >> 24;
            }
        }

        private static unsafe int LZ4HC_compress_hashChain64(
            LZ4HC_CCtx_internal* ctx,
            byte* src,
            byte* dst,
            int* srcSizePtr,
            int maxOutputSize,
            int maxNbAttempts,
            limitedOutput_directive limit,
            dictCtx_directive dict
        ) {
            int inputSize = *srcSizePtr;
            bool patternAnalysis = (maxNbAttempts > 128);   /* levels 9+ */

            byte* ip = (byte*)src;
            byte* anchor = ip;
            byte* iend = ip + inputSize;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = (iend - LASTLITERALS);

            byte* optr = (byte*)dst;
            byte* op = (byte*)dst;
            byte* oend = op + maxOutputSize;

            byte* start0;
            byte* start2 = null;
            byte* start3 = null;
            LZ4HC_match_t m0, m1, m2, m3;
            LZ4HC_match_t nomatch = new() { off = 0, len = 0, back = 0 };

            /* init */
            //DEBUGLOG(5, "LZ4HC_compress_hashChain (dict?=>%i)", dict);

            /* preconditions verifications */
            Debug.Assert(*srcSizePtr >= 1);
            Debug.Assert(src != null);
            Debug.Assert(maxOutputSize >= 1);
            Debug.Assert(dst != null);

            *srcSizePtr = 0;
            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;                  /* Hack for support LZ4 format restriction */
            if (inputSize < LZ4_minLength) goto _last_literals;             /* Input too small, no compression (all literals) */

            /* Main Loop */
            while (ip <= mflimit) {
                m1 = LZ4HC_InsertAndFindBestMatch64(ctx, ip, matchlimit, maxNbAttempts, patternAnalysis, dict);
                if (m1.len < MINMATCH) { ip++; continue; }

                /* saved, in case we would skip too much */
                start0 = ip; m0 = m1;

            _Search2:
                //DEBUGLOG(7, "_Search2 (currently found match of size %i)", m1.len);
                if (ip + m1.len <= mflimit) {
                    start2 = ip + m1.len - 2;
                    m2 = LZ4HC_InsertAndGetWiderMatch64(ctx,
                                    start2, ip + 0, matchlimit, m1.len,
                                    maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
                    start2 += m2.back;
                }
                else {
                    m2 = nomatch;  /* do not search further */
                }

                if (m2.len <= m1.len) { /* No better match => encode ML1 immediately */
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m1.len, m1.off,
                            limit, oend))
                        goto _dest_overflow;
                    continue;
                }

                if (start0 < ip) {   /* first match was skipped at least once */
                    if (start2 < ip + m0.len) {  /* squeezing ML1 between ML0(original ML1) and ML2 */
                        ip = start0; m1 = m0;  /* restore initial Match1 */
                    }
                }

                /* Here, start0==ip */
                if ((start2 - ip) < 3) {  /* First Match too small : removed */
                    ip = start2;
                    m1 = m2;
                    goto _Search2;
                }

            _Search3:
                if ((start2 - ip) < OPTIMAL_ML) {
                    int correction;
                    int new_ml = m1.len;
                    if (new_ml > OPTIMAL_ML) new_ml = OPTIMAL_ML;
                    if (ip + new_ml > start2 + m2.len - MINMATCH)
                        new_ml = (int)(start2 - ip) + m2.len - MINMATCH;
                    correction = new_ml - (int)(start2 - ip);
                    if (correction > 0) {
                        start2 += correction;
                        m2.len -= correction;
                    }
                }

                if (start2 + m2.len <= mflimit) {
                    start3 = start2 + m2.len - 3;
                    m3 = LZ4HC_InsertAndGetWiderMatch64(ctx,
                                    start3, start2, matchlimit, m2.len,
                                    maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
                    start3 += m3.back;
                }
                else {
                    m3 = nomatch;  /* do not search further */
                }

                if (m3.len <= m2.len) {  /* No better match => encode ML1 and ML2 */
                    /* ip & ref are known; Now for ml */
                    if (start2 < ip + m1.len) m1.len = (int)(start2 - ip);
                    /* Now, encode 2 sequences */
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m1.len, m1.off,
                            limit, oend))
                        goto _dest_overflow;
                    ip = start2;
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m2.len, m2.off,
                            limit, oend)) {
                        m1 = m2;
                        goto _dest_overflow;
                    }
                    continue;
                }

                if (start3 < ip + m1.len + 3) {  /* Not enough space for match 2 : remove it */
                    if (start3 >= (ip + m1.len)) {  /* can write Seq1 immediately ==> Seq2 is removed, so Seq3 becomes Seq1 */
                        if (start2 < ip + m1.len) {
                            int correction = (int)(ip + m1.len - start2);
                            start2 += correction;
                            m2.len -= correction;
                            if (m2.len < MINMATCH) {
                                start2 = start3;
                                m2 = m3;
                            }
                        }

                        optr = op;
                        if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                                m1.len, m1.off,
                                limit, oend))
                            goto _dest_overflow;
                        ip = start3;
                        m1 = m3;

                        start0 = start2;
                        m0 = m2;
                        goto _Search2;
                    }

                    start2 = start3;
                    m2 = m3;
                    goto _Search3;
                }

                /*
                * OK, now we have 3 ascending matches;
                * let's write the first one ML1.
                * ip & ref are known; Now decide ml.
                */
                if (start2 < ip + m1.len) {
                    if ((start2 - ip) < OPTIMAL_ML) {
                        int correction;
                        if (m1.len > OPTIMAL_ML) m1.len = OPTIMAL_ML;
                        if (ip + m1.len > start2 + m2.len - MINMATCH)
                            m1.len = (int)(start2 - ip) + m2.len - MINMATCH;
                        correction = m1.len - (int)(start2 - ip);
                        if (correction > 0) {
                            start2 += correction;
                            m2.len -= correction;
                        }
                    }
                    else {
                        m1.len = (int)(start2 - ip);
                    }
                }
                optr = op;
                if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                        m1.len, m1.off,
                        limit, oend))
                    goto _dest_overflow;

                /* ML2 becomes ML1 */
                ip = start2; m1 = m2;

                /* ML3 becomes ML2 */
                start2 = start3; m2 = m3;

                /* let's find a new ML3 */
                goto _Search3;
            }

        _last_literals:
            /* Encode Last Literals */
            {
                ulong lastRunSize = (ulong)(iend - anchor);  /* literals */
                ulong llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                ulong totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) return 0;
                    /* adapt lastRunSize to fill 'dest' */
                    lastRunSize = (ulong)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize;  /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    ulong accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Buffer.MemoryCopy(anchor, op, lastRunSize, lastRunSize); //LZ4_memcpy(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            *srcSizePtr = (int)(((byte*)ip) - src);
            return (int)(((byte*)op) - dst);

        _dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : @ip, @anchor, @optr and @m1 must be set correctly */
                ulong ll = (ulong)(ip - anchor);
                ulong ll_addbytes = (ll + 240) / 255;
                ulong ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence overflowing");
                op = optr;  /* restore correct out pointer */
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    ulong bytesLeftForMl = (ulong)(maxLitPos - (op + ll_totalCost));
                    ulong maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < long.MaxValue); Debug.Assert(m1.len >= 0);
                    if ((ulong)m1.len > maxMlSize) m1.len = (int)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + m1.len >= MFLIMIT) {
                        LZ4HC_encodeSequence(&ip, &op, &anchor, m1.len, m1.off, limitedOutput_directive.notLimited, oend);
                    }
                }
                goto _last_literals;
            }
            /* compression failed */
            return 0;
        }

        private static unsafe int LZ4HC_compress_hashChain32(
            LZ4HC_CCtx_internal* ctx,
            byte* src,
            byte* dst,
            int* srcSizePtr,
            int maxOutputSize,
            int maxNbAttempts,
            limitedOutput_directive limit,
            dictCtx_directive dict
        ) {
            int inputSize = *srcSizePtr;
            bool patternAnalysis = (maxNbAttempts > 128);   /* levels 9+ */

            byte* ip = (byte*)src;
            byte* anchor = ip;
            byte* iend = ip + inputSize;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = (iend - LASTLITERALS);

            byte* optr = (byte*)dst;
            byte* op = (byte*)dst;
            byte* oend = op + maxOutputSize;

            byte* start0;
            byte* start2 = null;
            byte* start3 = null;
            LZ4HC_match_t m0, m1, m2, m3;
            LZ4HC_match_t nomatch = new() { off = 0, len = 0, back = 0 };

            /* init */
            //DEBUGLOG(5, "LZ4HC_compress_hashChain (dict?=>%i)", dict);

            /* preconditions verifications */
            Debug.Assert(*srcSizePtr >= 1);
            Debug.Assert(src != null);
            Debug.Assert(maxOutputSize >= 1);
            Debug.Assert(dst != null);

            *srcSizePtr = 0;
            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;                  /* Hack for support LZ4 format restriction */
            if (inputSize < LZ4_minLength) goto _last_literals;             /* Input too small, no compression (all literals) */

            /* Main Loop */
            while (ip <= mflimit) {
                m1 = LZ4HC_InsertAndFindBestMatch32(ctx, ip, matchlimit, maxNbAttempts, patternAnalysis, dict);
                if (m1.len < MINMATCH) { ip++; continue; }

                /* saved, in case we would skip too much */
                start0 = ip; m0 = m1;

            _Search2:
                //DEBUGLOG(7, "_Search2 (currently found match of size %i)", m1.len);
                if (ip + m1.len <= mflimit) {
                    start2 = ip + m1.len - 2;
                    m2 = LZ4HC_InsertAndGetWiderMatch32(ctx,
                                    start2, ip + 0, matchlimit, m1.len,
                                    maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
                    start2 += m2.back;
                }
                else {
                    m2 = nomatch;  /* do not search further */
                }

                if (m2.len <= m1.len) { /* No better match => encode ML1 immediately */
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m1.len, m1.off,
                            limit, oend))
                        goto _dest_overflow;
                    continue;
                }

                if (start0 < ip) {   /* first match was skipped at least once */
                    if (start2 < ip + m0.len) {  /* squeezing ML1 between ML0(original ML1) and ML2 */
                        ip = start0; m1 = m0;  /* restore initial Match1 */
                    }
                }

                /* Here, start0==ip */
                if ((start2 - ip) < 3) {  /* First Match too small : removed */
                    ip = start2;
                    m1 = m2;
                    goto _Search2;
                }

            _Search3:
                if ((start2 - ip) < OPTIMAL_ML) {
                    int correction;
                    int new_ml = m1.len;
                    if (new_ml > OPTIMAL_ML) new_ml = OPTIMAL_ML;
                    if (ip + new_ml > start2 + m2.len - MINMATCH)
                        new_ml = (int)(start2 - ip) + m2.len - MINMATCH;
                    correction = new_ml - (int)(start2 - ip);
                    if (correction > 0) {
                        start2 += correction;
                        m2.len -= correction;
                    }
                }

                if (start2 + m2.len <= mflimit) {
                    start3 = start2 + m2.len - 3;
                    m3 = LZ4HC_InsertAndGetWiderMatch32(ctx,
                                    start3, start2, matchlimit, m2.len,
                                    maxNbAttempts, patternAnalysis, false, dict, HCfavor_e.favorCompressionRatio);
                    start3 += m3.back;
                }
                else {
                    m3 = nomatch;  /* do not search further */
                }

                if (m3.len <= m2.len) {  /* No better match => encode ML1 and ML2 */
                    /* ip & ref are known; Now for ml */
                    if (start2 < ip + m1.len) m1.len = (int)(start2 - ip);
                    /* Now, encode 2 sequences */
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m1.len, m1.off,
                            limit, oend))
                        goto _dest_overflow;
                    ip = start2;
                    optr = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                            m2.len, m2.off,
                            limit, oend)) {
                        m1 = m2;
                        goto _dest_overflow;
                    }
                    continue;
                }

                if (start3 < ip + m1.len + 3) {  /* Not enough space for match 2 : remove it */
                    if (start3 >= (ip + m1.len)) {  /* can write Seq1 immediately ==> Seq2 is removed, so Seq3 becomes Seq1 */
                        if (start2 < ip + m1.len) {
                            int correction = (int)(ip + m1.len - start2);
                            start2 += correction;
                            m2.len -= correction;
                            if (m2.len < MINMATCH) {
                                start2 = start3;
                                m2 = m3;
                            }
                        }

                        optr = op;
                        if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                                m1.len, m1.off,
                                limit, oend))
                            goto _dest_overflow;
                        ip = start3;
                        m1 = m3;

                        start0 = start2;
                        m0 = m2;
                        goto _Search2;
                    }

                    start2 = start3;
                    m2 = m3;
                    goto _Search3;
                }

                /*
                * OK, now we have 3 ascending matches;
                * let's write the first one ML1.
                * ip & ref are known; Now decide ml.
                */
                if (start2 < ip + m1.len) {
                    if ((start2 - ip) < OPTIMAL_ML) {
                        int correction;
                        if (m1.len > OPTIMAL_ML) m1.len = OPTIMAL_ML;
                        if (ip + m1.len > start2 + m2.len - MINMATCH)
                            m1.len = (int)(start2 - ip) + m2.len - MINMATCH;
                        correction = m1.len - (int)(start2 - ip);
                        if (correction > 0) {
                            start2 += correction;
                            m2.len -= correction;
                        }
                    }
                    else {
                        m1.len = (int)(start2 - ip);
                    }
                }
                optr = op;
                if (LZ4HC_encodeSequence(&ip, &op, &anchor,
                        m1.len, m1.off,
                        limit, oend))
                    goto _dest_overflow;

                /* ML2 becomes ML1 */
                ip = start2; m1 = m2;

                /* ML3 becomes ML2 */
                start2 = start3; m2 = m3;

                /* let's find a new ML3 */
                goto _Search3;
            }

        _last_literals:
            /* Encode Last Literals */
            {
                uint lastRunSize = (uint)(iend - anchor);  /* literals */
                uint llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                uint totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) return 0;
                    /* adapt lastRunSize to fill 'dest' */
                    lastRunSize = (uint)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize;  /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    uint accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Unsafe.CopyBlock(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            *srcSizePtr = (int)(((byte*)ip) - src);
            return (int)(((byte*)op) - dst);

        _dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : @ip, @anchor, @optr and @m1 must be set correctly */
                uint ll = (uint)(ip - anchor);
                uint ll_addbytes = (ll + 240) / 255;
                uint ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence overflowing");
                op = optr;  /* restore correct out pointer */
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    uint bytesLeftForMl = (uint)(maxLitPos - (op + ll_totalCost));
                    uint maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < int.MaxValue); Debug.Assert(m1.len >= 0);
                    if ((uint)m1.len > maxMlSize) m1.len = (int)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + m1.len >= MFLIMIT) {
                        LZ4HC_encodeSequence(&ip, &op, &anchor, m1.len, m1.off, limitedOutput_directive.notLimited, oend);
                    }
                }
                goto _last_literals;
            }
            /* compression failed */
            return 0;
        }

        private static unsafe LZ4HC_match_t LZ4HC_InsertAndFindBestMatch64(LZ4HC_CCtx_internal* hc4,   /* Index table will be updated */
                      byte* ip, byte* iLimit,
                      int maxNbAttempts,
                      bool patternAnalysis,
                      dictCtx_directive dict) {
            //DEBUGLOG(7, "LZ4HC_InsertAndFindBestMatch");
            /* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
             * but this won't be the case here, as we define iLowLimit==ip,
             * so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
            return LZ4HC_InsertAndGetWiderMatch64(hc4, ip, ip, iLimit, MINMATCH - 1, maxNbAttempts, patternAnalysis, false /*chainSwap*/, dict, HCfavor_e.favorCompressionRatio);
        }

        private static unsafe LZ4HC_match_t LZ4HC_InsertAndFindBestMatch32(LZ4HC_CCtx_internal* hc4,   /* Index table will be updated */
                       byte* ip, byte* iLimit,
                       int maxNbAttempts,
                       bool patternAnalysis,
                       dictCtx_directive dict) {
            //DEBUGLOG(7, "LZ4HC_InsertAndFindBestMatch");
            /* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
             * but this won't be the case here, as we define iLowLimit==ip,
             * so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
            return LZ4HC_InsertAndGetWiderMatch32(hc4, ip, ip, iLimit, MINMATCH - 1, maxNbAttempts, patternAnalysis, false /*chainSwap*/, dict, HCfavor_e.favorCompressionRatio);
        }

        private static unsafe LZ4HC_match_t LZ4HC_InsertAndGetWiderMatch64(
            LZ4HC_CCtx_internal* hc4,
            byte* ip,
            byte* iLowLimit, byte* iHighLimit,
            int longest,
            int maxNbAttempts,
            bool patternAnalysis, bool chainSwap,
            dictCtx_directive dict,
            HCfavor_e favorDecSpeed) {

            ushort* chainTable = hc4->chainTable;
            uint* hashTable = hc4->hashTable;
            LZ4HC_CCtx_internal* dictCtx = hc4->dictCtx;
            byte* prefixPtr = hc4->prefixStart;
            uint prefixIdx = hc4->dictLimit;
            uint ipIndex = (uint)(ip - prefixPtr) + prefixIdx;
            bool withinStartDistance = (hc4->lowLimit + (LZ4_DISTANCE_MAX + 1) > ipIndex);
            uint lowestMatchIndex = (withinStartDistance) ? hc4->lowLimit : ipIndex - LZ4_DISTANCE_MAX;
            byte* dictStart = hc4->dictStart;
            uint dictIdx = hc4->lowLimit;
            byte* dictEnd = dictStart + prefixIdx - dictIdx;
            int lookBackLength = (int)(ip - iLowLimit);
            int nbAttempts = maxNbAttempts;
            uint matchChainPos = 0;
            uint pattern = LZ4_read32(ip);
            uint matchIndex;
            repeat_state_e repeat = repeat_state_e.rep_untested;
            ulong srcPatternLength = 0;
            int offset = 0, sBack = 0;

            //DEBUGLOG(7, "LZ4HC_InsertAndGetWiderMatch");
            /* First Match */
            LZ4HC_Insert(hc4, ip);  /* insert all prior positions up to ip (excluded) */
            matchIndex = hashTable[LZ4HC_hashPtr(ip)];
            //DEBUGLOG(7, "First candidate match for pos %u found at index %u / %u (lowestMatchIndex)",
            //            ipIndex, matchIndex, lowestMatchIndex);

            while ((matchIndex >= lowestMatchIndex) && (nbAttempts > 0)) {
                int matchLength = 0;
                nbAttempts--;
                Debug.Assert(matchIndex < ipIndex);
                if (favorDecSpeed != HCfavor_e.favorCompressionRatio && (ipIndex - matchIndex < 8)) {
                    /* do nothing:
                     * favorDecSpeed intentionally skips matches with offset < 8 */
                }
                else if (matchIndex >= prefixIdx) {   /* within current Prefix */
                    byte* matchPtr = prefixPtr + (matchIndex - prefixIdx);
                    Debug.Assert(matchPtr < ip);
                    Debug.Assert(longest >= 1);
                    if (LZ4_read16(iLowLimit + longest - 1) == LZ4_read16(matchPtr - lookBackLength + longest - 1)) {
                        if (LZ4_read32(matchPtr) == pattern) {
                            int back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, prefixPtr) : 0;
                            matchLength = MINMATCH + (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, iHighLimit);
                            matchLength -= back;
                            if (matchLength > longest) {
                                longest = matchLength;
                                offset = (int)(ipIndex - matchIndex);
                                sBack = back;
                                //DEBUGLOG(7, "Found match of len=%i within prefix, offset=%i, back=%i", longest, offset, -back);
                                //HEX_CMP(7, ip + back, ip + back - offset, (ulong)matchLength);
                            }
                        }
                    }
                }
                else {   /* lowestMatchIndex <= matchIndex < dictLimit : within Ext Dict */
                    byte* matchPtr = dictStart + (matchIndex - dictIdx);
                    Debug.Assert(matchIndex >= dictIdx);
                    if ((matchIndex <= prefixIdx - 4)
                      && (LZ4_read32(matchPtr) == pattern)) {
                        int back = 0;
                        byte* vLimit = ip + (prefixIdx - matchIndex);
                        if (vLimit > iHighLimit) vLimit = iHighLimit;
                        matchLength = (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, vLimit) + MINMATCH;
                        if ((ip + matchLength == vLimit) && (vLimit < iHighLimit))
                            matchLength += (int)LZ4_count(ip + matchLength, prefixPtr, iHighLimit);
                        back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictStart) : 0;
                        matchLength -= back;
                        if (matchLength > longest) {
                            longest = matchLength;
                            offset = (int)(ipIndex - matchIndex);
                            sBack = back;
                            //DEBUGLOG(7, "Found match of len=%i within dict, offset=%i, back=%i", longest, offset, -back);
                            //HEX_CMP(7, ip + back, matchPtr + back, (ulong)matchLength);
                        }
                    }
                }

                if (chainSwap && matchLength == longest) {   /* better match => select a better chain */
                    Debug.Assert(lookBackLength == 0);   /* search forward only */
                    if (matchIndex + (uint)longest <= ipIndex) {
                        int kTrigger = 4;
                        uint distanceToNextMatch = 1;
                        int end = longest - MINMATCH + 1;
                        int step = 1;
                        int accel = 1 << kTrigger;
                        int pos;
                        for (pos = 0; pos < end; pos += step) {
                            uint candidateDist = DELTANEXTU16(chainTable, matchIndex + (uint)pos);
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
                    uint distNextMatch = DELTANEXTU16(chainTable, matchIndex);
                    if (patternAnalysis && distNextMatch == 1 && matchChainPos == 0) {
                        uint matchCandidateIdx = matchIndex - 1;
                        /* may be a repeated pattern */
                        if (repeat == repeat_state_e.rep_untested) {
                            if (((pattern & 0xFFFF) == (pattern >> 16))
                              & ((pattern & 0xFF) == (pattern >> 24))) {
                                //DEBUGLOG(7, "Repeat pattern detected, char %02X", pattern >> 24);
                                repeat = repeat_state_e.rep_confirmed;
                                srcPatternLength = LZ4HC_countPattern64(ip + 8, iHighLimit, pattern) + 8;
                            }
                            else {
                                repeat = repeat_state_e.rep_not;
                            }
                        }
                        if ((repeat == repeat_state_e.rep_confirmed) && (matchCandidateIdx >= lowestMatchIndex)
                          && LZ4HC_protectDictEnd(prefixIdx, matchCandidateIdx)) {
                            bool extDict = matchCandidateIdx < prefixIdx;
                            byte* matchPtr = extDict ? dictStart + (matchCandidateIdx - dictIdx) : prefixPtr + (matchCandidateIdx - prefixIdx);
                            if (LZ4_read32(matchPtr) == pattern) {  /* good candidate */
                                byte* iLimit = extDict ? dictEnd : iHighLimit;
                                ulong forwardPatternLength = LZ4HC_countPattern64(matchPtr + 8, iLimit, pattern) + 8;
                                if (extDict && matchPtr + forwardPatternLength == iLimit) {
                                    uint rotatedPattern = LZ4HC_rotatePattern64(forwardPatternLength, pattern);
                                    forwardPatternLength += LZ4HC_countPattern64(prefixPtr, iHighLimit, rotatedPattern);
                                }
                                {
                                    byte* lowestMatchPtr = extDict ? dictStart : prefixPtr;
                                    ulong backLength = LZ4HC_reverseCountPattern(matchPtr, lowestMatchPtr, pattern);
                                    ulong currentSegmentLength;
                                    if (!extDict
                                      && matchPtr - backLength == prefixPtr
                                      && dictIdx < prefixIdx) {
                                        uint rotatedPattern = LZ4HC_rotatePattern64((uint)(-(int)backLength), pattern);
                                        backLength += LZ4HC_reverseCountPattern(dictEnd, dictStart, rotatedPattern);
                                    }
                                    /* Limit backLength not go further than lowestMatchIndex */
                                    backLength = matchCandidateIdx - Math.Max(matchCandidateIdx - (uint)backLength, lowestMatchIndex);
                                    Debug.Assert(matchCandidateIdx - backLength >= lowestMatchIndex);
                                    currentSegmentLength = backLength + forwardPatternLength;
                                    /* Adjust to end of pattern if the source pattern fits, otherwise the beginning of the pattern */
                                    if ((currentSegmentLength >= srcPatternLength)   /* current pattern segment large enough to contain full srcPatternLength */
                                      && (forwardPatternLength <= srcPatternLength)) { /* haven't reached this position yet */
                                        uint newMatchIndex = matchCandidateIdx + (uint)forwardPatternLength - (uint)srcPatternLength;  /* best position, full pattern, might be followed by more match */
                                        if (LZ4HC_protectDictEnd(prefixIdx, newMatchIndex))
                                            matchIndex = newMatchIndex;
                                        else {
                                            /* Can only happen if started in the prefix */
                                            Debug.Assert(newMatchIndex >= prefixIdx - 3 && newMatchIndex < prefixIdx && !extDict);
                                            matchIndex = prefixIdx;
                                        }
                                    }
                                    else {
                                        uint newMatchIndex = matchCandidateIdx - (uint)backLength;   /* farthest position in current segment, will find a match of length currentSegmentLength + maybe some back */
                                        if (!LZ4HC_protectDictEnd(prefixIdx, newMatchIndex)) {
                                            Debug.Assert(newMatchIndex >= prefixIdx - 3 && newMatchIndex < prefixIdx && !extDict);
                                            matchIndex = prefixIdx;
                                        }
                                        else {
                                            matchIndex = newMatchIndex;
                                            if (lookBackLength == 0) {  /* no back possible */
                                                ulong maxML = Math.Min(currentSegmentLength, srcPatternLength);
                                                if ((ulong)longest < maxML) {
                                                    Debug.Assert(prefixPtr - prefixIdx + matchIndex != ip);
                                                    if ((ulong)(ip - prefixPtr) + prefixIdx - matchIndex > LZ4_DISTANCE_MAX) break;
                                                    Debug.Assert(maxML < 2 * GB);
                                                    longest = (int)maxML;
                                                    offset = (int)(ipIndex - matchIndex);
                                                    Debug.Assert(sBack == 0);
                                                    //DEBUGLOG(7, "Found repeat pattern match of len=%i, offset=%i", longest, offset);
                                                }
                                                {
                                                    uint distToNextPattern = DELTANEXTU16(chainTable, matchIndex);
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
                matchIndex -= DELTANEXTU16(chainTable, matchIndex + matchChainPos);

            }  /* while ((matchIndex>=lowestMatchIndex) && (nbAttempts)) */

            if (dict == dictCtx_directive.usingDictCtxHc
              && nbAttempts > 0
              && withinStartDistance) {
                ulong dictEndOffset = (ulong)(dictCtx->end - dictCtx->prefixStart) + dictCtx->dictLimit;
                uint dictMatchIndex = dictCtx->hashTable[LZ4HC_hashPtr(ip)];
                Debug.Assert(dictEndOffset <= 1 * GB);
                matchIndex = dictMatchIndex + lowestMatchIndex - (uint)dictEndOffset;
                //if (dictMatchIndex > 0) DEBUGLOG(7, "dictEndOffset = %zu, dictMatchIndex = %u => relative matchIndex = %i", dictEndOffset, dictMatchIndex, (int)dictMatchIndex - (int)dictEndOffset);
                while (ipIndex - matchIndex <= LZ4_DISTANCE_MAX && nbAttempts-- != 0) {
                    byte* matchPtr = dictCtx->prefixStart - dictCtx->dictLimit + dictMatchIndex;

                    if (LZ4_read32(matchPtr) == pattern) {
                        int mlt;
                        int back = 0;
                        byte* vLimit = ip + (dictEndOffset - dictMatchIndex);
                        if (vLimit > iHighLimit) vLimit = iHighLimit;
                        mlt = (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, vLimit) + MINMATCH;
                        back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictCtx->prefixStart) : 0;
                        mlt -= back;
                        if (mlt > longest) {
                            longest = mlt;
                            offset = (int)(ipIndex - matchIndex);
                            sBack = back;
                            //DEBUGLOG(7, "found match of length %i within extDictCtx", longest);
                        }
                    }

                    {
                        uint nextOffset = DELTANEXTU16(dictCtx->chainTable, dictMatchIndex);
                        dictMatchIndex -= nextOffset;
                        matchIndex -= nextOffset;
                    }
                }
            }

            {
                LZ4HC_match_t md;
                Debug.Assert(longest >= 0);
                md.len = longest;
                md.off = offset;
                md.back = sBack;
                return md;
            }
        }

        private static unsafe LZ4HC_match_t LZ4HC_InsertAndGetWiderMatch32(
            LZ4HC_CCtx_internal* hc4,
            byte* ip,
            byte* iLowLimit, byte* iHighLimit,
            int longest,
            int maxNbAttempts,
            bool patternAnalysis, bool chainSwap,
            dictCtx_directive dict,
            HCfavor_e favorDecSpeed) {

            ushort* chainTable = hc4->chainTable;
            uint* hashTable = hc4->hashTable;
            LZ4HC_CCtx_internal* dictCtx = hc4->dictCtx;
            byte* prefixPtr = hc4->prefixStart;
            uint prefixIdx = hc4->dictLimit;
            uint ipIndex = (uint)(ip - prefixPtr) + prefixIdx;
            bool withinStartDistance = (hc4->lowLimit + (LZ4_DISTANCE_MAX + 1) > ipIndex);
            uint lowestMatchIndex = (withinStartDistance) ? hc4->lowLimit : ipIndex - LZ4_DISTANCE_MAX;
            byte* dictStart = hc4->dictStart;
            uint dictIdx = hc4->lowLimit;
            byte* dictEnd = dictStart + prefixIdx - dictIdx;
            int lookBackLength = (int)(ip - iLowLimit);
            int nbAttempts = maxNbAttempts;
            uint matchChainPos = 0;
            uint pattern = LZ4_read32(ip);
            uint matchIndex;
            repeat_state_e repeat = repeat_state_e.rep_untested;
            uint srcPatternLength = 0;
            int offset = 0, sBack = 0;

            //DEBUGLOG(7, "LZ4HC_InsertAndGetWiderMatch");
            /* First Match */
            LZ4HC_Insert(hc4, ip);  /* insert all prior positions up to ip (excluded) */
            matchIndex = hashTable[LZ4HC_hashPtr(ip)];
            //DEBUGLOG(7, "First candidate match for pos %u found at index %u / %u (lowestMatchIndex)",
            //            ipIndex, matchIndex, lowestMatchIndex);

            while ((matchIndex >= lowestMatchIndex) && (nbAttempts > 0)) {
                int matchLength = 0;
                nbAttempts--;
                Debug.Assert(matchIndex < ipIndex);
                if (favorDecSpeed != HCfavor_e.favorCompressionRatio && (ipIndex - matchIndex < 8)) {
                    /* do nothing:
                     * favorDecSpeed intentionally skips matches with offset < 8 */
                }
                else if (matchIndex >= prefixIdx) {   /* within current Prefix */
                    byte* matchPtr = prefixPtr + (matchIndex - prefixIdx);
                    Debug.Assert(matchPtr < ip);
                    Debug.Assert(longest >= 1);
                    if (LZ4_read16(iLowLimit + longest - 1) == LZ4_read16(matchPtr - lookBackLength + longest - 1)) {
                        if (LZ4_read32(matchPtr) == pattern) {
                            int back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, prefixPtr) : 0;
                            matchLength = MINMATCH + (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, iHighLimit);
                            matchLength -= back;
                            if (matchLength > longest) {
                                longest = matchLength;
                                offset = (int)(ipIndex - matchIndex);
                                sBack = back;
                                //DEBUGLOG(7, "Found match of len=%i within prefix, offset=%i, back=%i", longest, offset, -back);
                                //HEX_CMP(7, ip + back, ip + back - offset, (uint)matchLength);
                            }
                        }
                    }
                }
                else {   /* lowestMatchIndex <= matchIndex < dictLimit : within Ext Dict */
                    byte* matchPtr = dictStart + (matchIndex - dictIdx);
                    Debug.Assert(matchIndex >= dictIdx);
                    if ((matchIndex <= prefixIdx - 4)
                      && (LZ4_read32(matchPtr) == pattern)) {
                        int back = 0;
                        byte* vLimit = ip + (prefixIdx - matchIndex);
                        if (vLimit > iHighLimit) vLimit = iHighLimit;
                        matchLength = (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, vLimit) + MINMATCH;
                        if ((ip + matchLength == vLimit) && (vLimit < iHighLimit))
                            matchLength += (int)LZ4_count(ip + matchLength, prefixPtr, iHighLimit);
                        back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictStart) : 0;
                        matchLength -= back;
                        if (matchLength > longest) {
                            longest = matchLength;
                            offset = (int)(ipIndex - matchIndex);
                            sBack = back;
                            //DEBUGLOG(7, "Found match of len=%i within dict, offset=%i, back=%i", longest, offset, -back);
                            //HEX_CMP(7, ip + back, matchPtr + back, (uint)matchLength);
                        }
                    }
                }

                if (chainSwap && matchLength == longest) {   /* better match => select a better chain */
                    Debug.Assert(lookBackLength == 0);   /* search forward only */
                    if (matchIndex + (uint)longest <= ipIndex) {
                        int kTrigger = 4;
                        uint distanceToNextMatch = 1;
                        int end = longest - MINMATCH + 1;
                        int step = 1;
                        int accel = 1 << kTrigger;
                        int pos;
                        for (pos = 0; pos < end; pos += step) {
                            uint candidateDist = DELTANEXTU16(chainTable, matchIndex + (uint)pos);
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
                    uint distNextMatch = DELTANEXTU16(chainTable, matchIndex);
                    if (patternAnalysis && distNextMatch == 1 && matchChainPos == 0) {
                        uint matchCandidateIdx = matchIndex - 1;
                        /* may be a repeated pattern */
                        if (repeat == repeat_state_e.rep_untested) {
                            if (((pattern & 0xFFFF) == (pattern >> 16))
                              & ((pattern & 0xFF) == (pattern >> 24))) {
                                //DEBUGLOG(7, "Repeat pattern detected, char %02X", pattern >> 24);
                                repeat = repeat_state_e.rep_confirmed;
                                srcPatternLength = LZ4HC_countPattern32(ip + 4, iHighLimit, pattern) + 4;
                            }
                            else {
                                repeat = repeat_state_e.rep_not;
                            }
                        }
                        if ((repeat == repeat_state_e.rep_confirmed) && (matchCandidateIdx >= lowestMatchIndex)
                          && LZ4HC_protectDictEnd(prefixIdx, matchCandidateIdx)) {
                            bool extDict = matchCandidateIdx < prefixIdx;
                            byte* matchPtr = extDict ? dictStart + (matchCandidateIdx - dictIdx) : prefixPtr + (matchCandidateIdx - prefixIdx);
                            if (LZ4_read32(matchPtr) == pattern) {  /* good candidate */
                                byte* iLimit = extDict ? dictEnd : iHighLimit;
                                uint forwardPatternLength = LZ4HC_countPattern32(matchPtr + 4, iLimit, pattern) + 4;
                                if (extDict && matchPtr + forwardPatternLength == iLimit) {
                                    uint rotatedPattern = LZ4HC_rotatePattern32(forwardPatternLength, pattern);
                                    forwardPatternLength += LZ4HC_countPattern32(prefixPtr, iHighLimit, rotatedPattern);
                                }
                                {
                                    byte* lowestMatchPtr = extDict ? dictStart : prefixPtr;
                                    uint backLength = LZ4HC_reverseCountPattern(matchPtr, lowestMatchPtr, pattern);
                                    uint currentSegmentLength;
                                    if (!extDict
                                      && matchPtr - backLength == prefixPtr
                                      && dictIdx < prefixIdx) {
                                        uint rotatedPattern = LZ4HC_rotatePattern32((uint)(-(int)backLength), pattern);
                                        backLength += LZ4HC_reverseCountPattern(dictEnd, dictStart, rotatedPattern);
                                    }
                                    /* Limit backLength not go further than lowestMatchIndex */
                                    backLength = matchCandidateIdx - Math.Max(matchCandidateIdx - (uint)backLength, lowestMatchIndex);
                                    Debug.Assert(matchCandidateIdx - backLength >= lowestMatchIndex);
                                    currentSegmentLength = backLength + forwardPatternLength;
                                    /* Adjust to end of pattern if the source pattern fits, otherwise the beginning of the pattern */
                                    if ((currentSegmentLength >= srcPatternLength)   /* current pattern segment large enough to contain full srcPatternLength */
                                      && (forwardPatternLength <= srcPatternLength)) { /* haven't reached this position yet */
                                        uint newMatchIndex = matchCandidateIdx + (uint)forwardPatternLength - (uint)srcPatternLength;  /* best position, full pattern, might be followed by more match */
                                        if (LZ4HC_protectDictEnd(prefixIdx, newMatchIndex))
                                            matchIndex = newMatchIndex;
                                        else {
                                            /* Can only happen if started in the prefix */
                                            Debug.Assert(newMatchIndex >= prefixIdx - 3 && newMatchIndex < prefixIdx && !extDict);
                                            matchIndex = prefixIdx;
                                        }
                                    }
                                    else {
                                        uint newMatchIndex = matchCandidateIdx - (uint)backLength;   /* farthest position in current segment, will find a match of length currentSegmentLength + maybe some back */
                                        if (!LZ4HC_protectDictEnd(prefixIdx, newMatchIndex)) {
                                            Debug.Assert(newMatchIndex >= prefixIdx - 3 && newMatchIndex < prefixIdx && !extDict);
                                            matchIndex = prefixIdx;
                                        }
                                        else {
                                            matchIndex = newMatchIndex;
                                            if (lookBackLength == 0) {  /* no back possible */
                                                uint maxML = Math.Min(currentSegmentLength, srcPatternLength);
                                                if ((uint)longest < maxML) {
                                                    Debug.Assert(prefixPtr - prefixIdx + matchIndex != ip);
                                                    if ((uint)(ip - prefixPtr) + prefixIdx - matchIndex > LZ4_DISTANCE_MAX) break;
                                                    Debug.Assert(maxML < 2 * GB);
                                                    longest = (int)maxML;
                                                    offset = (int)(ipIndex - matchIndex);
                                                    Debug.Assert(sBack == 0);
                                                    //DEBUGLOG(7, "Found repeat pattern match of len=%i, offset=%i", longest, offset);
                                                }
                                                {
                                                    uint distToNextPattern = DELTANEXTU16(chainTable, matchIndex);
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
                matchIndex -= DELTANEXTU16(chainTable, matchIndex + matchChainPos);

            }  /* while ((matchIndex>=lowestMatchIndex) && (nbAttempts)) */

            if (dict == dictCtx_directive.usingDictCtxHc
              && nbAttempts > 0
              && withinStartDistance) {
                uint dictEndOffset = (uint)(dictCtx->end - dictCtx->prefixStart) + dictCtx->dictLimit;
                uint dictMatchIndex = dictCtx->hashTable[LZ4HC_hashPtr(ip)];
                Debug.Assert(dictEndOffset <= 1 * GB);
                matchIndex = dictMatchIndex + lowestMatchIndex - (uint)dictEndOffset;
                //if (dictMatchIndex > 0) DEBUGLOG(7, "dictEndOffset = %zu, dictMatchIndex = %u => relative matchIndex = %i", dictEndOffset, dictMatchIndex, (int)dictMatchIndex - (int)dictEndOffset);
                while (ipIndex - matchIndex <= LZ4_DISTANCE_MAX && nbAttempts-- != 0) {
                    byte* matchPtr = dictCtx->prefixStart - dictCtx->dictLimit + dictMatchIndex;

                    if (LZ4_read32(matchPtr) == pattern) {
                        int mlt;
                        int back = 0;
                        byte* vLimit = ip + (dictEndOffset - dictMatchIndex);
                        if (vLimit > iHighLimit) vLimit = iHighLimit;
                        mlt = (int)LZ4_count(ip + MINMATCH, matchPtr + MINMATCH, vLimit) + MINMATCH;
                        back = lookBackLength != 0 ? LZ4HC_countBack(ip, matchPtr, iLowLimit, dictCtx->prefixStart) : 0;
                        mlt -= back;
                        if (mlt > longest) {
                            longest = mlt;
                            offset = (int)(ipIndex - matchIndex);
                            sBack = back;
                            //DEBUGLOG(7, "found match of length %i within extDictCtx", longest);
                        }
                    }

                    {
                        uint nextOffset = DELTANEXTU16(dictCtx->chainTable, dictMatchIndex);
                        dictMatchIndex -= nextOffset;
                        matchIndex -= nextOffset;
                    }
                }
            }

            {
                LZ4HC_match_t md;
                Debug.Assert(longest >= 0);
                md.len = longest;
                md.off = offset;
                md.back = sBack;
                return md;
            }
        }

        private static unsafe uint LZ4HC_countPattern64(byte* ip, byte* iEnd, uint pattern32) {
            byte* iStart = ip;
            ulong pattern =
                    (ulong)pattern32 + (((ulong)pattern32) << (8 * 4));

            while (ip < iEnd - (8 - 1)) {
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
                uint bitOffset = (8 * 8) - 8;
                while (ip < iEnd) {
                    byte byteV = (byte)(pattern >> (int)bitOffset);
                    if (*ip != byteV) break;
                    ip++; bitOffset -= 8;
                }
            }

            return (uint)(ip - iStart);
        }

        private static unsafe uint LZ4HC_countPattern32(byte* ip, byte* iEnd, uint pattern32) {
            byte* iStart = ip;
            uint pattern = pattern32;

            while (ip < iEnd - (4 - 1)) {
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
                uint bitOffset = (4 * 8) - 8;
                while (ip < iEnd) {
                    byte byteV = (byte)(pattern >> (int)bitOffset);
                    if (*ip != byteV) break;
                    ip++; bitOffset -= 8;
                }
            }

            return (uint)(ip - iStart);
        }

        private static bool LZ4HC_protectDictEnd(uint dictLimit, uint matchIndex) {
            return ((uint)((dictLimit - 1) - matchIndex) >= 3);
        }

        private static uint LZ4HC_rotatePattern64(ulong rotate, uint pattern) {
            ulong bitsToRotate = (rotate & (4 - 1)) << 3;
            if (bitsToRotate == 0) return pattern;
            return LZ4HC_rotl32(pattern, (int)bitsToRotate);
        }

        private static uint LZ4HC_rotatePattern32(uint rotate, uint pattern) {
            uint bitsToRotate = (rotate & (4 - 1)) << 3;
            if (bitsToRotate == 0) return pattern;
            return LZ4HC_rotl32(pattern, (int)bitsToRotate);
        }

        private static uint LZ4HC_rotl32(uint x, int r) {
#if NET6_0_OR_GREATER
            return BitOperations.RotateLeft(x, r);
#else
            return (x << r) | (x >> (32 - r));
#endif
        }

        private static unsafe uint LZ4HC_reverseCountPattern(byte* ip, byte* iLow, uint pattern) {
            byte* iStart = ip;

            while (ip >= iLow + 4) {
                if (LZ4_read32(ip - 4) != pattern) break;
                ip -= 4;
            }
            {
                byte* bytePtr = (byte*)(&pattern) + 3; /* works for any endianness */
                while (ip > iLow) {
                    if (ip[-1] != *bytePtr) break;
                    ip--; bytePtr--;
                }
            }
            return (uint)(iStart - ip);
        }

        private static unsafe int LZ4HC_compress_optimal64(LZ4HC_CCtx_internal* ctx,
            byte* source,
            byte* dst,
            int* srcSizePtr,
            int dstCapacity,
            int nbSearches,
            ulong sufficient_len,
            limitedOutput_directive limit,
            bool fullUpdate,
            dictCtx_directive dict,
            HCfavor_e favorDecSpeed) {

            int retval = 0;
            LZ4HC_optimal_t[] opt = new LZ4HC_optimal_t[LZ4_OPT_NUM + TRAILING_LITERALS];   /* ~64 KB, which can be a bit large for some stacks... */

            byte* ip = (byte*)source;
            byte* anchor = ip;
            byte* iend = ip + *srcSizePtr;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = iend - LASTLITERALS;
            byte* op = (byte*)dst;
            byte* opSaved = (byte*)dst;
            byte* oend = op + dstCapacity;
            int ovml = MINMATCH;  /* overflow - last sequence */
            int ovoff = 0;

            /* init */
            //DEBUGLOG(5, "LZ4HC_compress_optimal(dst=%p, dstCapa=%u)", dst, (uint)dstCapacity);

            /* preconditions verifications */
            Debug.Assert(dstCapacity > 0);
            Debug.Assert(dst != null);
            Debug.Assert(*srcSizePtr > 0);
            Debug.Assert(source != null);

            *srcSizePtr = 0;
            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;   /* Hack for support LZ4 format restriction */
            if (sufficient_len >= LZ4_OPT_NUM) sufficient_len = LZ4_OPT_NUM - 1;

            /* Main Loop */
            while (ip <= mflimit) {
                int llen = (int)(ip - anchor);
                int best_mlen, best_off;
                int cur, last_match_pos = 0;

                LZ4HC_match_t firstMatch = LZ4HC_FindLongerMatch64(ctx, ip, matchlimit, MINMATCH - 1, nbSearches, dict, favorDecSpeed);
                if (firstMatch.len == 0) { ip++; continue; }

                if ((ulong)firstMatch.len > sufficient_len) {
                    /* good enough solution : immediate encoding */
                    int firstML = firstMatch.len;
                    opSaved = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor, firstML, firstMatch.off, limit, oend)) {  /* updates ip, op and anchor */
                        ovml = firstML;
                        ovoff = firstMatch.off;
                        goto _dest_overflow;
                    }
                    continue;
                }

                /* set prices for first positions (literals) */
                {
                    int rPos;
                    for (rPos = 0; rPos < MINMATCH; rPos++) {
                        int cost = LZ4HC_literalsPrice(llen + rPos);
                        opt[rPos].mlen = 1;
                        opt[rPos].off = 0;
                        opt[rPos].litlen = llen + rPos;
                        opt[rPos].price = cost;
                        //DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i) -- initial setup",
                        //            rPos, cost, opt[rPos].litlen);
                    }
                }
                /* set prices using initial match */
                {
                    int matchML = firstMatch.len;   /* necessarily < sufficient_len < LZ4_OPT_NUM */
                    int offset = firstMatch.off;
                    int mlen;
                    Debug.Assert(matchML < LZ4_OPT_NUM);
                    for (mlen = MINMATCH; mlen <= matchML; mlen++) {
                        int cost = LZ4HC_sequencePrice(llen, mlen);
                        opt[mlen].mlen = mlen;
                        opt[mlen].off = offset;
                        opt[mlen].litlen = llen;
                        opt[mlen].price = cost;
                        //DEBUGLOG(7, "rPos:%3i => price:%3i (matchlen=%i) -- initial setup",
                        //            mlen, cost, mlen);
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
                        //            last_match_pos + addLit, opt[last_match_pos + addLit].price, addLit);
                    }
                }

                /* check further positions */
                for (cur = 1; cur < last_match_pos; cur++) {
                    byte* curPtr = ip + cur;
                    LZ4HC_match_t newMatch;

                    if (curPtr > mflimit) break;
                    //DEBUGLOG(7, "rPos:%u[%u] vs [%u]%u",
                    //        cur, opt[cur].price, opt[cur + 1].price, cur + 1);
                    if (fullUpdate) {
                        /* not useful to search here if next position has same (or lower) cost */
                        if ((opt[cur + 1].price <= opt[cur].price)
                          /* in some cases, next position has same cost, but cost rises sharply after, so a small match would still be beneficial */
                          && (opt[cur + MINMATCH].price < opt[cur].price + 3/*min seq price*/))
                            continue;
                    }
                    else {
                        /* not useful to search here if next position has same (or lower) cost */
                        if (opt[cur + 1].price <= opt[cur].price) continue;
                    }

                    //DEBUGLOG(7, "search at rPos:%u", cur);
                    if (fullUpdate)
                        newMatch = LZ4HC_FindLongerMatch64(ctx, curPtr, matchlimit, MINMATCH - 1, nbSearches, dict, favorDecSpeed);
                    else
                        /* only test matches of minimum length; slightly faster, but misses a few bytes */
                        newMatch = LZ4HC_FindLongerMatch64(ctx, curPtr, matchlimit, last_match_pos - cur, nbSearches, dict, favorDecSpeed);
                    if (newMatch.len == 0) continue;

                    if (((ulong)newMatch.len > sufficient_len)
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
                        for (litlen = 1; litlen < MINMATCH; litlen++) {
                            int price = opt[cur].price - LZ4HC_literalsPrice(baseLitlen) + LZ4HC_literalsPrice(baseLitlen + litlen);
                            int pos = cur + litlen;
                            if (price < opt[pos].price) {
                                opt[pos].mlen = 1; /* literal */
                                opt[pos].off = 0;
                                opt[pos].litlen = baseLitlen + litlen;
                                opt[pos].price = price;
                                //DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i)",
                                //            pos, price, opt[pos].litlen);
                            }
                        }
                    }

                    /* set prices using match at position = cur */
                    {
                        int matchML = newMatch.len;
                        int ml = MINMATCH;

                        Debug.Assert(cur + newMatch.len < LZ4_OPT_NUM);
                        for (; ml <= matchML; ml++) {
                            int pos = cur + ml;
                            int offset = newMatch.off;
                            int price;
                            int ll;
                            //DEBUGLOG(7, "testing price rPos %i (last_match_pos=%i)",
                            //            pos, last_match_pos);
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
                                //            pos, price, ml);
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
                        Debug.Assert(ml >= MINMATCH);
                        Debug.Assert((offset >= 1) && (offset <= LZ4_DISTANCE_MAX));
                        opSaved = op;
                        if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, offset, limit, oend)) {  /* updates ip, op and anchor */
                            ovml = ml;
                            ovoff = offset;
                            goto _dest_overflow;
                        }
                    }
                }
            }  /* while (ip <= mflimit) */

        _last_literals:
            /* Encode Last Literals */
            {
                ulong lastRunSize = (ulong)(iend - anchor);  /* literals */
                ulong llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                ulong totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) { /* Check output limit */
                        retval = 0;
                        goto _return_label;
                    }
                    /* adapt lastRunSize to fill 'dst' */
                    lastRunSize = (ulong)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize; /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    ulong accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Buffer.MemoryCopy(anchor, op, lastRunSize, lastRunSize); //LZ4_memcpy(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            *srcSizePtr = (int)(((byte*)ip) - source);
            retval = (int)((byte*)op - dst);
            goto _return_label;

        _dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : ip, anchor, ovml and ovref must be set correctly */
                ulong ll = (ulong)(ip - anchor);
                ulong ll_addbytes = (ll + 240) / 255;
                ulong ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence overflowing (only %i bytes remaining)", (int)(oend - 1 - opSaved));
                op = opSaved;  /* restore correct out pointer */
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    ulong bytesLeftForMl = (ulong)(maxLitPos - (op + ll_totalCost));
                    ulong maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < long.MaxValue); Debug.Assert(ovml >= 0);
                    if ((ulong)ovml > maxMlSize) ovml = (int)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + ovml >= MFLIMIT) {
                        //DEBUGLOG(6, "Space to end : %i + ml (%i)", (int)((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1), ovml);
                        //DEBUGLOG(6, "Before : ip = %p, anchor = %p", ip, anchor);
                        LZ4HC_encodeSequence(&ip, &op, &anchor, ovml, ovoff, limitedOutput_directive.notLimited, oend);
                        //DEBUGLOG(6, "After : ip = %p, anchor = %p", ip, anchor);
                    }
                }
                goto _last_literals;
            }
        _return_label:
            return retval;
        }

        private static unsafe int LZ4HC_compress_optimal32(LZ4HC_CCtx_internal* ctx,
           byte* source,
           byte* dst,
           int* srcSizePtr,
           int dstCapacity,
           int nbSearches,
           uint sufficient_len,
           limitedOutput_directive limit,
           bool fullUpdate,
           dictCtx_directive dict,
           HCfavor_e favorDecSpeed) {

            int retval = 0;
            LZ4HC_optimal_t[] opt = new LZ4HC_optimal_t[LZ4_OPT_NUM + TRAILING_LITERALS];   /* ~64 KB, which can be a bit large for some stacks... */

            byte* ip = (byte*)source;
            byte* anchor = ip;
            byte* iend = ip + *srcSizePtr;
            byte* mflimit = iend - MFLIMIT;
            byte* matchlimit = iend - LASTLITERALS;
            byte* op = (byte*)dst;
            byte* opSaved = (byte*)dst;
            byte* oend = op + dstCapacity;
            int ovml = MINMATCH;  /* overflow - last sequence */
            int ovoff = 0;

            /* init */
            //DEBUGLOG(5, "LZ4HC_compress_optimal(dst=%p, dstCapa=%u)", dst, (uint)dstCapacity);

            /* preconditions verifications */
            Debug.Assert(dstCapacity > 0);
            Debug.Assert(dst != null);
            Debug.Assert(*srcSizePtr > 0);
            Debug.Assert(source != null);

            *srcSizePtr = 0;
            if (limit == limitedOutput_directive.fillOutput) oend -= LASTLITERALS;   /* Hack for support LZ4 format restriction */
            if (sufficient_len >= LZ4_OPT_NUM) sufficient_len = LZ4_OPT_NUM - 1;

            /* Main Loop */
            while (ip <= mflimit) {
                int llen = (int)(ip - anchor);
                int best_mlen, best_off;
                int cur, last_match_pos = 0;

                LZ4HC_match_t firstMatch = LZ4HC_FindLongerMatch32(ctx, ip, matchlimit, MINMATCH - 1, nbSearches, dict, favorDecSpeed);
                if (firstMatch.len == 0) { ip++; continue; }

                if ((uint)firstMatch.len > sufficient_len) {
                    /* good enough solution : immediate encoding */
                    int firstML = firstMatch.len;
                    opSaved = op;
                    if (LZ4HC_encodeSequence(&ip, &op, &anchor, firstML, firstMatch.off, limit, oend)) {  /* updates ip, op and anchor */
                        ovml = firstML;
                        ovoff = firstMatch.off;
                        goto _dest_overflow;
                    }
                    continue;
                }

                /* set prices for first positions (literals) */
                {
                    int rPos;
                    for (rPos = 0; rPos < MINMATCH; rPos++) {
                        int cost = LZ4HC_literalsPrice(llen + rPos);
                        opt[rPos].mlen = 1;
                        opt[rPos].off = 0;
                        opt[rPos].litlen = llen + rPos;
                        opt[rPos].price = cost;
                        //DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i) -- initial setup",
                        //            rPos, cost, opt[rPos].litlen);
                    }
                }
                /* set prices using initial match */
                {
                    int matchML = firstMatch.len;   /* necessarily < sufficient_len < LZ4_OPT_NUM */
                    int offset = firstMatch.off;
                    int mlen;
                    Debug.Assert(matchML < LZ4_OPT_NUM);
                    for (mlen = MINMATCH; mlen <= matchML; mlen++) {
                        int cost = LZ4HC_sequencePrice(llen, mlen);
                        opt[mlen].mlen = mlen;
                        opt[mlen].off = offset;
                        opt[mlen].litlen = llen;
                        opt[mlen].price = cost;
                        //DEBUGLOG(7, "rPos:%3i => price:%3i (matchlen=%i) -- initial setup",
                        //            mlen, cost, mlen);
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
                        //            last_match_pos + addLit, opt[last_match_pos + addLit].price, addLit);
                    }
                }

                /* check further positions */
                for (cur = 1; cur < last_match_pos; cur++) {
                    byte* curPtr = ip + cur;
                    LZ4HC_match_t newMatch;

                    if (curPtr > mflimit) break;
                    //DEBUGLOG(7, "rPos:%u[%u] vs [%u]%u",
                    //        cur, opt[cur].price, opt[cur + 1].price, cur + 1);
                    if (fullUpdate) {
                        /* not useful to search here if next position has same (or lower) cost */
                        if ((opt[cur + 1].price <= opt[cur].price)
                          /* in some cases, next position has same cost, but cost rises sharply after, so a small match would still be beneficial */
                          && (opt[cur + MINMATCH].price < opt[cur].price + 3/*min seq price*/))
                            continue;
                    }
                    else {
                        /* not useful to search here if next position has same (or lower) cost */
                        if (opt[cur + 1].price <= opt[cur].price) continue;
                    }

                    //DEBUGLOG(7, "search at rPos:%u", cur);
                    if (fullUpdate)
                        newMatch = LZ4HC_FindLongerMatch32(ctx, curPtr, matchlimit, MINMATCH - 1, nbSearches, dict, favorDecSpeed);
                    else
                        /* only test matches of minimum length; slightly faster, but misses a few bytes */
                        newMatch = LZ4HC_FindLongerMatch32(ctx, curPtr, matchlimit, last_match_pos - cur, nbSearches, dict, favorDecSpeed);
                    if (newMatch.len == 0) continue;

                    if (((uint)newMatch.len > sufficient_len)
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
                        for (litlen = 1; litlen < MINMATCH; litlen++) {
                            int price = opt[cur].price - LZ4HC_literalsPrice(baseLitlen) + LZ4HC_literalsPrice(baseLitlen + litlen);
                            int pos = cur + litlen;
                            if (price < opt[pos].price) {
                                opt[pos].mlen = 1; /* literal */
                                opt[pos].off = 0;
                                opt[pos].litlen = baseLitlen + litlen;
                                opt[pos].price = price;
                                //DEBUGLOG(7, "rPos:%3i => price:%3i (litlen=%i)",
                                //            pos, price, opt[pos].litlen);
                            }
                        }
                    }

                    /* set prices using match at position = cur */
                    {
                        int matchML = newMatch.len;
                        int ml = MINMATCH;

                        Debug.Assert(cur + newMatch.len < LZ4_OPT_NUM);
                        for (; ml <= matchML; ml++) {
                            int pos = cur + ml;
                            int offset = newMatch.off;
                            int price;
                            int ll;
                            //DEBUGLOG(7, "testing price rPos %i (last_match_pos=%i)",
                            //            pos, last_match_pos);
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
                                //            pos, price, ml);
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
                        Debug.Assert(ml >= MINMATCH);
                        Debug.Assert((offset >= 1) && (offset <= LZ4_DISTANCE_MAX));
                        opSaved = op;
                        if (LZ4HC_encodeSequence(&ip, &op, &anchor, ml, offset, limit, oend)) {  /* updates ip, op and anchor */
                            ovml = ml;
                            ovoff = offset;
                            goto _dest_overflow;
                        }
                    }
                }
            }  /* while (ip <= mflimit) */

        _last_literals:
            /* Encode Last Literals */
            {
                uint lastRunSize = (uint)(iend - anchor);  /* literals */
                uint llAdd = (lastRunSize + 255 - RUN_MASK) / 255;
                uint totalSize = 1 + llAdd + lastRunSize;
                if (limit == limitedOutput_directive.fillOutput) oend += LASTLITERALS;  /* restore correct value */
                if (limit != limitedOutput_directive.notLimited && (op + totalSize > oend)) {
                    if (limit == limitedOutput_directive.limitedOutput) { /* Check output limit */
                        retval = 0;
                        goto _return_label;
                    }
                    /* adapt lastRunSize to fill 'dst' */
                    lastRunSize = (uint)(oend - op) - 1 /*token*/;
                    llAdd = (lastRunSize + 256 - RUN_MASK) / 256;
                    lastRunSize -= llAdd;
                }
                //DEBUGLOG(6, "Final literal run : %i literals", (int)lastRunSize);
                ip = anchor + lastRunSize; /* can be != iend if limit==fillOutput */

                if (lastRunSize >= RUN_MASK) {
                    uint accumulator = lastRunSize - RUN_MASK;
                    *op++ = (RUN_MASK << ML_BITS);
                    for (; accumulator >= 255; accumulator -= 255) *op++ = 255;
                    *op++ = (byte)accumulator;
                }
                else {
                    *op++ = (byte)(lastRunSize << ML_BITS);
                }
                Unsafe.CopyBlock(op, anchor, lastRunSize);
                op += lastRunSize;
            }

            /* End */
            *srcSizePtr = (int)(((byte*)ip) - source);
            retval = (int)((byte*)op - dst);
            goto _return_label;

        _dest_overflow:
            if (limit == limitedOutput_directive.fillOutput) {
                /* Assumption : ip, anchor, ovml and ovref must be set correctly */
                uint ll = (uint)(ip - anchor);
                uint ll_addbytes = (ll + 240) / 255;
                uint ll_totalCost = 1 + ll_addbytes + ll;
                byte* maxLitPos = oend - 3; /* 2 for offset, 1 for token */
                //DEBUGLOG(6, "Last sequence overflowing (only %i bytes remaining)", (int)(oend - 1 - opSaved));
                op = opSaved;  /* restore correct out pointer */
                if (op + ll_totalCost <= maxLitPos) {
                    /* ll validated; now adjust match length */
                    uint bytesLeftForMl = (uint)(maxLitPos - (op + ll_totalCost));
                    uint maxMlSize = MINMATCH + (ML_MASK - 1) + (bytesLeftForMl * 255);
                    Debug.Assert(maxMlSize < int.MaxValue); Debug.Assert(ovml >= 0);
                    if ((uint)ovml > maxMlSize) ovml = (int)maxMlSize;
                    if ((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1 + ovml >= MFLIMIT) {
                        //DEBUGLOG(6, "Space to end : %i + ml (%i)", (int)((oend + LASTLITERALS) - (op + ll_totalCost + 2) - 1), ovml);
                        //DEBUGLOG(6, "Before : ip = %p, anchor = %p", ip, anchor);
                        LZ4HC_encodeSequence(&ip, &op, &anchor, ovml, ovoff, limitedOutput_directive.notLimited, oend);
                        //DEBUGLOG(6, "After : ip = %p, anchor = %p", ip, anchor);
                    }
                }
                goto _last_literals;
            }
        _return_label:
            return retval;
        }

        private static unsafe LZ4HC_match_t LZ4HC_FindLongerMatch64(LZ4HC_CCtx_internal* ctx,
                                         byte* ip, byte* iHighLimit,
                                        int minLen, int nbSearches,
                                         dictCtx_directive dict,
                                         HCfavor_e favorDecSpeed) {
            LZ4HC_match_t match0 = new LZ4HC_match_t { off = 0, len = 0, back = 0 };
            /* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
             * but this won't be the case here, as we define iLowLimit==ip,
            ** so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
            LZ4HC_match_t md = LZ4HC_InsertAndGetWiderMatch64(ctx, ip, ip, iHighLimit, minLen, nbSearches, true /*patternAnalysis*/, true /*chainSwap*/, dict, favorDecSpeed);
            Debug.Assert(md.back == 0);
            if (md.len <= minLen) return match0;
            if (favorDecSpeed != HCfavor_e.favorCompressionRatio) {
                if ((md.len > 18) & (md.len <= 36)) md.len = 18;   /* favor dec.speed (shortcut) */
            }
            return md;
        }

        private static unsafe LZ4HC_match_t LZ4HC_FindLongerMatch32(LZ4HC_CCtx_internal* ctx,
                                        byte* ip, byte* iHighLimit,
                                       int minLen, int nbSearches,
                                        dictCtx_directive dict,
                                        HCfavor_e favorDecSpeed) {
            LZ4HC_match_t match0 = new LZ4HC_match_t { off = 0, len = 0, back = 0 };
            /* note : LZ4HC_InsertAndGetWiderMatch() is able to modify the starting position of a match (*startpos),
             * but this won't be the case here, as we define iLowLimit==ip,
            ** so LZ4HC_InsertAndGetWiderMatch() won't be allowed to search past ip */
            LZ4HC_match_t md = LZ4HC_InsertAndGetWiderMatch32(ctx, ip, ip, iHighLimit, minLen, nbSearches, true /*patternAnalysis*/, true /*chainSwap*/, dict, favorDecSpeed);
            Debug.Assert(md.back == 0);
            if (md.len <= minLen) return match0;
            if (favorDecSpeed != HCfavor_e.favorCompressionRatio) {
                if ((md.len > 18) & (md.len <= 36)) md.len = 18;   /* favor dec.speed (shortcut) */
            }
            return md;
        }

        private static int LZ4HC_literalsPrice(int litlen) {
            int price = litlen;
            Debug.Assert(litlen >= 0);
            if (litlen >= (int)RUN_MASK)
                price += 1 + ((litlen - (int)RUN_MASK) / 255);
            return price;
        }

        private static int LZ4HC_sequencePrice(int litlen, int mlen) {
            int price = 1 + 2; /* token + 16-bit offset */
            Debug.Assert(litlen >= 0);
            Debug.Assert(mlen >= MINMATCH);

            price += LZ4HC_literalsPrice(litlen);

            if (mlen >= (int)(ML_MASK + MINMATCH))
                price += 1 + ((mlen - (int)(ML_MASK + MINMATCH)) / 255);

            return price;
        }

        internal static unsafe LZ4_stream* LZ4_createStream(out IDisposable disposable) {

            var lz4s = AllocWrapper.Alloc<LZ4_stream>(false, out disposable);
            if (lz4s == null) return null;
            LZ4_initStream(lz4s, sizeof(LZ4_stream));
            return lz4s;
        }

        internal static unsafe LZ4_streamHC* LZ4_createStreamHC(out IDisposable disposable) {

            var lz4s = AllocWrapper.Alloc<LZ4_streamHC>(true, out disposable);
            if (lz4s == null) return null;
            LZ4_setCompressionLevel(lz4s, LZ4HC_CLEVEL_DEFAULT);
            return lz4s;
        }

        internal static unsafe XXH32_state* XXH32_createState(out IDisposable disposable) {
            return AllocWrapper.Alloc<XXH32_state>(false, out disposable);
        }

        internal static unsafe LZ4_streamDecode* LZ4_createStreamDecode(out IDisposable disposable) {
            LZ4_streamDecode* lz4s = AllocWrapper.Alloc<LZ4_streamDecode>(true, out disposable);
            return lz4s;
        }

        internal static unsafe uint XXH32(void* input, uint len, uint seed) {

            XXH_endianess endian_detected = BitConverter.IsLittleEndian ? XXH_endianess.XXH_littleEndian : XXH_endianess.XXH_bigEndian;

            if ((endian_detected == XXH_endianess.XXH_littleEndian))
                return XXH32_endian_align(input, len, seed, XXH_endianess.XXH_littleEndian, XXH_alignment.XXH_unaligned);
            else
                return XXH32_endian_align(input, len, seed, XXH_endianess.XXH_bigEndian, XXH_alignment.XXH_unaligned);
        }


        internal static unsafe XXH_errorcode XXH32_reset(XXH32_state* statePtr, uint seed) {
            XXH32_state state;   /* using a local state to memcpy() in order to avoid strict-aliasing warnings */
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

        internal static unsafe XXH_errorcode XXH32_update(XXH32_state* state_in, void* input, uint len) {
            XXH_endianess endian_detected = BitConverter.IsLittleEndian ? XXH_endianess.XXH_littleEndian : XXH_endianess.XXH_bigEndian;

            if ((endian_detected == XXH_endianess.XXH_littleEndian))
                return XXH32_update_endian(state_in, input, len, XXH_endianess.XXH_littleEndian);
            else
                return XXH32_update_endian(state_in, input, len, XXH_endianess.XXH_bigEndian);
        }

        private static unsafe uint XXH32_endian_align(void* input, uint len, uint seed,
            XXH_endianess endian, XXH_alignment align) {

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

        private static unsafe XXH_errorcode XXH32_update_endian(XXH32_state* state, void* input, uint len, XXH_endianess endian) {
            if (input == null)
                return XXH_errorcode.XXH_ERROR;

            {
                byte* p = (byte*)input;
                byte* bEnd = p + len;

                state->total_len_32 += len;
                state->large_len |= ((len >= 16) | (state->total_len_32 >= 16)) ? 1u : 0;

                if (state->memsize + len < 16) {   /* fill in tmp buffer */
                    Unsafe.CopyBlock((byte*)(state->mem32) + state->memsize, input, len);
                    state->memsize += len;
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
                    Unsafe.CopyBlock(state->mem32, p, checked((uint)(bEnd - p)));
                    state->memsize = (uint)(bEnd - p);
                }
            }

            return XXH_errorcode.XXH_OK;
        }

        private static unsafe uint XXH_readLE32(void* ptr, XXH_endianess endian) {
            return XXH_readLE32_align(ptr, endian, XXH_alignment.XXH_unaligned);
        }

        private static uint XXH_rotl32(uint x, int r) {
#if NET6_0_OR_GREATER
            return BitOperations.RotateLeft(x, r);
#else
            return (x << r) | (x >> (32 - r));
#endif
        }

        private static unsafe uint XXH32_finalize(uint h32, void* ptr, uint len, XXH_endianess endian, XXH_alignment align) {
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
            Debug.Assert(false);
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
#if NET6_0_OR_GREATER
            return BinaryPrimitives.ReverseEndianness(x);
#else
            return ((x << 24) & 0xff000000) |
                     ((x << 8) & 0x00ff0000) |
                     ((x >> 8) & 0x0000ff00) |
                     ((x >> 24) & 0x000000ff);
#endif
        }

        private static uint XXH32_avalanche(uint h32) {
            h32 ^= h32 >> 15;
            h32 *= PRIME32_2;
            h32 ^= h32 >> 13;
            h32 *= PRIME32_3;
            h32 ^= h32 >> 16;
            return (h32);
        }

        internal static unsafe int LZ4_loadDict(LZ4_stream* LZ4_dict, byte* dictionary, int dictSize) {
            return LZ4_loadDict_internal(LZ4_dict, dictionary, dictSize, LoadDict_mode_e._ld_fast);
        }

        internal static unsafe int LZ4_loadDict_internal(LZ4_stream* LZ4_dict, byte* dictionary, int dictSize, LoadDict_mode_e _ld) {

            LZ4_stream_t_internal* dict = &LZ4_dict->internal_donotuse;
            tableType_t tableType = tableType_t.byU32;
            byte* p = (byte*)dictionary;
            byte* dictEnd = p + dictSize;
            uint idx32;

            //DEBUGLOG(4, "LZ4_loadDict (%i bytes from %p into %p)", dictSize, (void*)dictionary, (void*)LZ4_dict);

            /* It's necessary to reset the context,
             * and not just continue it with prepareTable()
             * to avoid any risk of generating overflowing matchIndex
             * when compressing using this dictionary */
            LZ4_resetStream(LZ4_dict);

            /* We always increment the offset by 64 KB, since, if the dict is longer,
             * we truncate it to the last 64k, and if it's shorter, we still want to
             * advance by a whole window length so we can provide the guarantee that
             * there are only valid offsets in the window, which allows an optimization
             * in LZ4_compress_fast_continue() where it uses noDictIssue even when the
             * dictionary isn't a full 64k. */
            dict->currentOffset += 64 * KB;

            if (dictSize < (int)HASH_UNIT) {
                return 0;
            }

            if ((dictEnd - p) > 64 * KB) p = dictEnd - 64 * KB;
            dict->dictionary = p;
            dict->dictSize = (uint)(dictEnd - p);
            dict->tableType = (uint)tableType;
            idx32 = dict->currentOffset - dict->dictSize;

            while (p <= dictEnd - HASH_UNIT) {
                uint h = LZ4_hashPosition(p, tableType);
                /* Note: overwriting => favors positions end of dictionary */
                LZ4_putIndexOnHash(idx32, h, dict->hashTable, tableType);
                p += 3; idx32 += 3;
            }

            if (_ld == LoadDict_mode_e._ld_slow) {
                /* Fill hash table with additional references, to improve compression capability */
                p = dict->dictionary;
                idx32 = dict->currentOffset - dict->dictSize;
                while (p <= dictEnd - HASH_UNIT) {
                    uint h = LZ4_hashPosition(p, tableType);
                    uint limit = dict->currentOffset - 64 * KB;
                    if (LZ4_getIndexOnHash(h, dict->hashTable, tableType) <= limit) {
                        /* Note: not overwriting => favors positions beginning of dictionary */
                        LZ4_putIndexOnHash(idx32, h, dict->hashTable, tableType);
                    }
                    p++; idx32++;
                }
            }

            return (int)dict->dictSize;
        }

        private static unsafe void LZ4_resetStream(LZ4_stream* LZ4_stream) {
            //DEBUGLOG(5, "LZ4_resetStream (ctx:%p)", (void*)LZ4_stream);
            Unsafe.InitBlock(LZ4_stream, 0, (uint)sizeof(LZ4_stream)); // MEM_INIT(LZ4_stream, 0, sizeof(LZ4_stream_t_internal));
        }

        internal static unsafe int LZ4_loadDictHC(LZ4_streamHC* LZ4_streamHCPtr,
                             byte* dictionary, int dictSize) {
            if (IntPtr.Size == 8) {
                return LZ4_loadDictHC64(LZ4_streamHCPtr, dictionary, dictSize);
            }
            else {
                return LZ4_loadDictHC32(LZ4_streamHCPtr, dictionary, dictSize);
            }
        }

        private static unsafe int LZ4_loadDictHC64(LZ4_streamHC* LZ4_streamHCPtr,
                             byte* dictionary, int dictSize) {
            LZ4HC_CCtx_internal* ctxPtr = &LZ4_streamHCPtr->internal_donotuse;
            cParams_t cp;
            //DEBUGLOG(4, "LZ4_loadDictHC(ctx:%p, dict:%p, dictSize:%d, clevel=%d)", LZ4_streamHCPtr, dictionary, dictSize, ctxPtr->compressionLevel);
            Debug.Assert(dictSize >= 0);
            Debug.Assert(LZ4_streamHCPtr != null);
            if (dictSize > 64 * KB) {
                dictionary += (ulong)dictSize - 64 * KB;
                dictSize = (int)(64 * KB);
            }
            /* need a full initialization, there are bad side-effects when using resetFast() */
            {
                int cLevel = ctxPtr->compressionLevel;
                LZ4_initStreamHC(LZ4_streamHCPtr, sizeof(LZ4_streamHC));
                LZ4_setCompressionLevel(LZ4_streamHCPtr, cLevel);
                cp = LZ4HC_getCLevelParams(cLevel);
            }
            LZ4HC_init_internal(ctxPtr, (byte*)dictionary);
            ctxPtr->end = (byte*)dictionary + dictSize;
            if (cp.strat == lz4hc_strat_e.lz4mid) {
                LZ4MID_fillHTable64(ctxPtr, dictionary, (ulong)dictSize);
            }
            else {
                if (dictSize >= LZ4HC_HASHSIZE) LZ4HC_Insert(ctxPtr, ctxPtr->end - 3);
            }
            return dictSize;
        }

        private static unsafe int LZ4_loadDictHC32(LZ4_streamHC* LZ4_streamHCPtr,
                             byte* dictionary, int dictSize) {
            LZ4HC_CCtx_internal* ctxPtr = &LZ4_streamHCPtr->internal_donotuse;
            cParams_t cp;
            //DEBUGLOG(4, "LZ4_loadDictHC(ctx:%p, dict:%p, dictSize:%d, clevel=%d)", LZ4_streamHCPtr, dictionary, dictSize, ctxPtr->compressionLevel);
            Debug.Assert(dictSize >= 0);
            Debug.Assert(LZ4_streamHCPtr != null);
            if (dictSize > 64 * KB) {
                dictionary += (uint)dictSize - 64 * KB;
                dictSize = (int)(64 * KB);
            }
            /* need a full initialization, there are bad side-effects when using resetFast() */
            {
                int cLevel = ctxPtr->compressionLevel;
                LZ4_initStreamHC(LZ4_streamHCPtr, sizeof(LZ4_streamHC));
                LZ4_setCompressionLevel(LZ4_streamHCPtr, cLevel);
                cp = LZ4HC_getCLevelParams(cLevel);
            }
            LZ4HC_init_internal(ctxPtr, (byte*)dictionary);
            ctxPtr->end = (byte*)dictionary + dictSize;
            if (cp.strat == lz4hc_strat_e.lz4mid) {
                LZ4MID_fillHTable32(ctxPtr, dictionary, (uint)dictSize);
            }
            else {
                if (dictSize >= LZ4HC_HASHSIZE) LZ4HC_Insert(ctxPtr, ctxPtr->end - 3);
            }
            return dictSize;
        }

        private static unsafe void LZ4MID_fillHTable64(LZ4HC_CCtx_internal* cctx, void* dict, ulong size) {
            uint* hash4Table = cctx->hashTable;
            uint* hash8Table = hash4Table + LZ4MID_HASHTABLESIZE;
            byte* prefixPtr = (byte*)dict;
            uint prefixIdx = cctx->dictLimit;
            uint target = prefixIdx + (uint)size - LZ4MID_HASHSIZE;
            uint idx = cctx->nextToUpdate;

            unsafe void ADDPOS4(byte* _p, uint _idx) { LZ4MID_addPosition(hash4Table, LZ4MID_hash4Ptr(_p), _idx); }

            unsafe void ADDPOS8(byte* _p, uint _idx) { LZ4MID_addPosition(hash8Table, LZ4MID_hash8Ptr(_p), _idx); }
            Debug.Assert(dict == cctx->prefixStart);
            //DEBUGLOG(4, "LZ4MID_fillHTable (size:%zu)", size);
            if (size <= LZ4MID_HASHSIZE)
                return;

            for (; idx < target; idx += 3) {
                ADDPOS4(prefixPtr + idx - prefixIdx, idx);
                ADDPOS8(prefixPtr + idx + 1 - prefixIdx, idx + 1);
            }

            idx = (size > 32 * KB + LZ4MID_HASHSIZE) ? target - 32 * KB : cctx->nextToUpdate;
            for (; idx < target; idx += 1) {
                ADDPOS8(prefixPtr + idx - prefixIdx, idx);
            }

            cctx->nextToUpdate = target;
        }

        private static unsafe void LZ4MID_fillHTable32(LZ4HC_CCtx_internal* cctx, void* dict, uint size) {
            uint* hash4Table = cctx->hashTable;
            uint* hash8Table = hash4Table + LZ4MID_HASHTABLESIZE;
            byte* prefixPtr = (byte*)dict;
            uint prefixIdx = cctx->dictLimit;
            uint target = prefixIdx + (uint)size - LZ4MID_HASHSIZE;
            uint idx = cctx->nextToUpdate;

            unsafe void ADDPOS4(byte* _p, uint _idx) { LZ4MID_addPosition(hash4Table, LZ4MID_hash4Ptr(_p), _idx); }

            unsafe void ADDPOS8(byte* _p, uint _idx) { LZ4MID_addPosition(hash8Table, LZ4MID_hash8Ptr(_p), _idx); }
            Debug.Assert(dict == cctx->prefixStart);
            //DEBUGLOG(4, "LZ4MID_fillHTable (size:%zu)", size);
            if (size <= LZ4MID_HASHSIZE)
                return;

            for (; idx < target; idx += 3) {
                ADDPOS4(prefixPtr + idx - prefixIdx, idx);
                ADDPOS8(prefixPtr + idx + 1 - prefixIdx, idx + 1);
            }

            idx = (size > 32 * KB + LZ4MID_HASHSIZE) ? target - 32 * KB : cctx->nextToUpdate;
            for (; idx < target; idx += 1) {
                ADDPOS8(prefixPtr + idx - prefixIdx, idx);
            }

            cctx->nextToUpdate = target;
        }

        internal static unsafe int LZ4_compress_fast_continue(LZ4_stream* LZ4_stream,
            byte* source, byte* dest,
            int inputSize, int maxOutputSize,
            int acceleration) {

            tableType_t tableType = tableType_t.byU32;
            LZ4_stream_t_internal* streamPtr = &LZ4_stream->internal_donotuse;
            byte* dictEnd = streamPtr->dictSize != 0 ? (byte*)streamPtr->dictionary + streamPtr->dictSize : null;

            //DEBUGLOG(5, "LZ4_compress_fast_continue (inputSize=%i, dictSize=%u)", inputSize, streamPtr->dictSize);

            LZ4_renormDictT(streamPtr, inputSize);   /* fix index overflow */
            if (acceleration < 1) acceleration = LZ4_ACCELERATION_DEFAULT;
            if (acceleration > LZ4_ACCELERATION_MAX) acceleration = LZ4_ACCELERATION_MAX;

            /* invalidate tiny dictionaries */
            if ((streamPtr->dictSize < 4)     /* tiny dictionary : not enough for a hash */
              && (dictEnd != source)           /* prefix mode */
              && (inputSize > 0)               /* tolerance : don't lose history, in case next invocation would use prefix mode */
              && (streamPtr->dictCtx == null)  /* usingDictCtx */
              ) {
                //DEBUGLOG(5, "LZ4_compress_fast_continue: dictSize(%u) at addr:%p is too small", streamPtr->dictSize, (void*)streamPtr->dictionary);
                /* remove dictionary existence from history, to employ faster prefix mode */
                streamPtr->dictSize = 0;
                streamPtr->dictionary = (byte*)source;
                dictEnd = source;
            }

            /* Check overlapping input/dictionary space */
            {
                byte* sourceEnd = source + inputSize;
                if ((sourceEnd > (byte*)streamPtr->dictionary) && (sourceEnd < dictEnd)) {
                    streamPtr->dictSize = (uint)(dictEnd - sourceEnd);
                    if (streamPtr->dictSize > 64 * KB) streamPtr->dictSize = 64 * KB;
                    if (streamPtr->dictSize < 4) streamPtr->dictSize = 0;
                    streamPtr->dictionary = (byte*)dictEnd - streamPtr->dictSize;
                }
            }

            /* prefix mode : source data follows dictionary */
            if (dictEnd == source) {
                if ((streamPtr->dictSize < 64 * KB) && (streamPtr->dictSize < streamPtr->currentOffset))
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
                     * to offsets between dictCtx->currentOffset - 64 KB and
                     * dictCtx->currentOffset - dictCtx->dictSize. This makes it safe
                     * to use noDictIssue even when the dict isn't a full 64 KB.
                     */
                    if (inputSize > 4 * KB) {
                        /* For compressing large blobs, it is faster to pay the setup
                         * cost to copy the dictionary's tables into the active context,
                         * so that the compression loop is only looking into one table.
                         */
                        Unsafe.CopyBlock(streamPtr, streamPtr->dictCtx, (uint)sizeof(LZ4_stream_t_internal));
                        result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingExtDict, dictIssue_directive.noDictIssue, acceleration);
                    }
                    else {
                        result = LZ4_compress_generic(streamPtr, source, dest, inputSize, null, maxOutputSize, limitedOutput_directive.limitedOutput, tableType, dict_directive.usingDictCtx, dictIssue_directive.noDictIssue, acceleration);
                    }
                }
                else {  /* small data <= 4 KB */
                    if ((streamPtr->dictSize < 64 * KB) && (streamPtr->dictSize < streamPtr->currentOffset)) {
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
                uint delta = LZ4_dict->currentOffset - 64 * KB;
                byte* dictEnd = LZ4_dict->dictionary + LZ4_dict->dictSize;
                int i;
                //DEBUGLOG(4, "LZ4_renormDictT");
                for (i = 0; i < LZ4_HASH_SIZE_U32; i++) {
                    if (LZ4_dict->hashTable[i] < delta) LZ4_dict->hashTable[i] = 0;
                    else LZ4_dict->hashTable[i] -= delta;
                }
                LZ4_dict->currentOffset = 64 * KB;
                if (LZ4_dict->dictSize > 64 * KB) LZ4_dict->dictSize = 64 * KB;
                LZ4_dict->dictionary = dictEnd - LZ4_dict->dictSize;
            }
        }

        internal static unsafe int LZ4_compress_HC_continue(LZ4_streamHC* LZ4_streamHCPtr, byte* src, byte* dst, int srcSize, int dstCapacity) {
            //DEBUGLOG(5, "LZ4_compress_HC_continue");
            if (dstCapacity < LZ4_compressBound(srcSize))
                return LZ4_compressHC_continue_generic(LZ4_streamHCPtr, src, dst, &srcSize, dstCapacity, limitedOutput_directive.limitedOutput);
            else
                return LZ4_compressHC_continue_generic(LZ4_streamHCPtr, src, dst, &srcSize, dstCapacity, limitedOutput_directive.notLimited);
        }

        private static unsafe int LZ4_compressHC_continue_generic(LZ4_streamHC* LZ4_streamHCPtr,
            byte* src, byte* dst,
            int* srcSizePtr, int dstCapacity,
            limitedOutput_directive limit) {

            LZ4HC_CCtx_internal* ctxPtr = &LZ4_streamHCPtr->internal_donotuse;
            //DEBUGLOG(5, "LZ4_compressHC_continue_generic(ctx=%p, src=%p, srcSize=%d, limit=%d)",
            //            LZ4_streamHCPtr, src, *srcSizePtr, limit);
            Debug.Assert(ctxPtr != null);
            /* auto-init if forgotten */
            if (ctxPtr->prefixStart == null)
                LZ4HC_init_internal(ctxPtr, (byte*)src);

            /* Check overflow */
            if ((ulong)(ctxPtr->end - ctxPtr->prefixStart) + ctxPtr->dictLimit > 2 * GB) {
                ulong dictSize = (ulong)(ctxPtr->end - ctxPtr->prefixStart);
                if (dictSize > 64 * KB) dictSize = 64 * KB;
                LZ4_loadDictHC(LZ4_streamHCPtr, (byte*)(ctxPtr->end) - dictSize, (int)dictSize);
            }

            /* Check if blocks follow each other */
            if ((byte*)src != ctxPtr->end)
                LZ4HC_setExternalDict(ctxPtr, (byte*)src);

            /* Check overlapping input/dictionary space */
            {
                byte* sourceEnd = (byte*)src + *srcSizePtr;
                byte* dictBegin = ctxPtr->dictStart;
                byte* dictEnd = ctxPtr->dictStart + (ctxPtr->dictLimit - ctxPtr->lowLimit);
                if ((sourceEnd > dictBegin) && ((byte*)src < dictEnd)) {
                    if (sourceEnd > dictEnd) sourceEnd = dictEnd;
                    ctxPtr->lowLimit += (uint)(sourceEnd - ctxPtr->dictStart);
                    ctxPtr->dictStart += (uint)(sourceEnd - ctxPtr->dictStart);
                    /* invalidate dictionary is it's too small */
                    if (ctxPtr->dictLimit - ctxPtr->lowLimit < LZ4HC_HASHSIZE) {
                        ctxPtr->lowLimit = ctxPtr->dictLimit;
                        ctxPtr->dictStart = ctxPtr->prefixStart;
                    }
                }
            }

            return LZ4HC_compress_generic(ctxPtr, src, dst, srcSizePtr, dstCapacity, ctxPtr->compressionLevel, limit);
        }

        internal static unsafe int LZ4_setStreamDecode(LZ4_streamDecode* LZ4_streamDecode, byte* dictionary, int dictSize) {
            LZ4_streamDecode_t_internal* lz4sd = &LZ4_streamDecode->internal_donotuse;
            lz4sd->prefixSize = (uint)dictSize;
            if (dictSize != 0) {
                Debug.Assert(dictionary != null);
                lz4sd->prefixEnd = (byte*)dictionary + dictSize;
            }
            else {
                lz4sd->prefixEnd = (byte*)dictionary;
            }
            lz4sd->externalDict = null;
            lz4sd->extDictSize = 0;
            return 1;
        }

        internal static unsafe int LZ4_decompress_safe_continue(LZ4_streamDecode* LZ4_streamDecode, byte* source, byte* dest, int compressedSize, int maxOutputSize) {

            LZ4_streamDecode_t_internal* lz4sd = &LZ4_streamDecode->internal_donotuse;
            int result;

            if (lz4sd->prefixSize == 0) {
                /* The first call, no dictionary yet. */
                Debug.Assert(lz4sd->extDictSize == 0);
                result = LZ4_decompress_safe(source, dest, compressedSize, maxOutputSize);
                if (result <= 0) return result;
                lz4sd->prefixSize = (uint)result;
                lz4sd->prefixEnd = (byte*)dest + result;
            }
            else if (lz4sd->prefixEnd == (byte*)dest) {
                /* They're rolling the current segment. */
                if (lz4sd->prefixSize >= 64 * KB - 1)
                    result = LZ4_decompress_safe_withPrefix64k(source, dest, compressedSize, maxOutputSize);
                else if (lz4sd->extDictSize == 0)
                    result = LZ4_decompress_safe_withSmallPrefix(source, dest, compressedSize, maxOutputSize,
                                                                 lz4sd->prefixSize);
                else
                    result = LZ4_decompress_safe_doubleDict(source, dest, compressedSize, maxOutputSize,
                                                            lz4sd->prefixSize, lz4sd->externalDict, lz4sd->extDictSize);
                if (result <= 0) return result;
                lz4sd->prefixSize += (uint)result;
                lz4sd->prefixEnd += result;
            }
            else {
                /* The buffer wraps around, or they're switching to another buffer. */
                lz4sd->extDictSize = lz4sd->prefixSize;
                lz4sd->externalDict = lz4sd->prefixEnd - lz4sd->extDictSize;
                result = LZ4_decompress_safe_forceExtDict(source, dest, compressedSize, maxOutputSize,
                                                          lz4sd->externalDict, lz4sd->extDictSize);
                if (result <= 0) return result;
                lz4sd->prefixSize = (uint)result;
                lz4sd->prefixEnd = (byte*)dest + result;
            }

            return result;
        }

        private static unsafe int LZ4_decompress_safe_withPrefix64k(byte* source, byte* dest, int compressedSize, int maxOutputSize) {
            return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                earlyEnd_directive.decode_full_block, dict_directive.withPrefix64k,
                (byte*)dest - 64 * KB, null, 0);
        }

        private static unsafe int LZ4_decompress_safe_withSmallPrefix(byte* source, byte* dest, int compressedSize, int maxOutputSize,
            uint prefixSize) {

            return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                earlyEnd_directive.decode_full_block, dict_directive.noDict,
                (byte*)dest - prefixSize, null, 0);
        }

        private static unsafe int LZ4_decompress_safe_doubleDict(byte* source, byte* dest, int compressedSize, int maxOutputSize,
            uint prefixSize, void* dictStart, uint dictSize) {

            return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                earlyEnd_directive.decode_full_block, dict_directive.usingExtDict,
                (byte*)dest - prefixSize, (byte*)dictStart, dictSize);
        }

        private static unsafe int LZ4_decompress_safe_forceExtDict(byte* source, byte* dest,
            int compressedSize, int maxOutputSize,
            void* dictStart, uint dictSize) {

            //DEBUGLOG(5, "LZ4_decompress_safe_forceExtDict");
            return LZ4_decompress_generic(source, dest, compressedSize, maxOutputSize,
                earlyEnd_directive.decode_full_block, dict_directive.usingExtDict,
                (byte*)dest, (byte*)dictStart, dictSize);
        }
    }
}
