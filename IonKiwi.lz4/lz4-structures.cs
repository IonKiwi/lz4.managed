using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using size_t = System.UInt32;

namespace IonKiwi.lz4 {

	internal unsafe struct LZ4_streamDecode_t_internal {
		public byte* externalDict;
		public size_t extDictSize;
		public byte* prefixEnd;
		public size_t prefixSize;
	}

	internal unsafe struct LZ4_streamDecode_u {
		public fixed ulong table[4];
		public LZ4_streamDecode_t_internal internal_donotuse;
	}

	internal enum endCondition_directive {
		endOnOutputSize = 0,
		endOnInputSize = 1
	}

	internal enum earlyEnd_directive {
		decode_full_block = 0,
		partial_decode = 1
	}

	internal enum dict_directive {
		noDict = 0,
		withPrefix64k,
		usingExtDict,
		usingDictCtx
	}

	internal enum variable_length_error {
		loop_error = -2,
		initial_error = -1,
		ok = 0
	}

	internal unsafe struct LZ4_stream_t_internal {
		public fixed uint hashTable[lz4.LZ4_HASH_SIZE_U32];
		public uint currentOffset;
		public ushort dirty;
		public ushort tableType;
		public byte* dictionary;
		public LZ4_stream_t_internal* dictCtx;
		public uint dictSize;
	};

	internal unsafe struct LZ4_stream_u {
		public fixed ulong table[lz4.LZ4_STREAMSIZE_U64];
		public LZ4_stream_t_internal internal_donotuse;
	}

	internal unsafe struct LZ4_streamHC_u {
		public fixed size_t table[lz4.LZ4_STREAMHCSIZE_SIZET];
		public LZ4HC_CCtx_internal internal_donotuse;
	};

	internal unsafe struct LZ4HC_CCtx_internal {
		public fixed uint hashTable[lz4.LZ4HC_HASHTABLESIZE];
		public fixed ushort chainTable[lz4.LZ4HC_MAXD];
		public byte* end;         /* next block here to continue on current prefix */
		public byte* basePtr;        /* All index relative to this position */
		public byte* dictBase;    /* alternate base for extDict */
		public uint dictLimit;       /* below that point, need extDict */
		public uint lowLimit;        /* below that point, no more dict */
		public uint nextToUpdate;    /* index from which to continue dictionary update */
		public short compressionLevel;
		public sbyte favorDecSpeed;   /* favor decompression speed if this flag set,
                                   otherwise, favor compression ratio */
		public sbyte dirty;           /* stream has to be fully reset if this flag is set */
		public LZ4HC_CCtx_internal* dictCtx;
	};

	internal enum tableType_t {
		clearedTable = 0,
		byPtr,
		byU32,
		byU16
	}

	internal enum limitedOutput_directive {
		notLimited = 0,
		limitedOutput = 1,
		fillOutput = 2
	}

	internal enum dictIssue_directive {
		noDictIssue = 0,
		dictSmall
	}

	internal enum dictCtx_directive {
		noDictCtx,
		usingDictCtxHc
	}

	internal enum lz4hc_strat_e {
		lz4hc,
		lz4opt
	}

	internal struct cParams_t {
		public lz4hc_strat_e strat;
		public uint nbSearches;
		public uint targetLength;
	}

	internal enum repeat_state_e {
		rep_untested,
		rep_not,
		rep_confirmed
	}

	internal enum HCfavor_e {
		favorCompressionRatio = 0,
		favorDecompressionSpeed
	}

	internal struct LZ4HC_optimal_t {
		public int price;
		public int off;
		public int mlen;
		public int litlen;
	}

	internal struct LZ4HC_match_t {
		public int off;
		public int len;
	}

	internal unsafe struct XXH32_state_s {
		public uint total_len_32;
		public uint large_len;
		public uint v1;
		public uint v2;
		public uint v3;
		public uint v4;
		public fixed uint mem32[4];
		public uint memsize;
		public uint reserved;   /* never read nor write, might be removed in a future version */
	};

	internal enum XXH_errorcode {
		XXH_OK = 0,
		XXH_ERROR
	}

	internal enum XXH_endianess {
		XXH_bigEndian = 0,
		XXH_littleEndian = 1
	}

	internal enum XXH_alignment {
		XXH_aligned,
		XXH_unaligned
	}
}
