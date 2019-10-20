using System;
using System.Collections.Generic;
using System.Text;

namespace IonKiwi.lz4 {
	[Flags]
	public enum LZ4FrameChecksumMode {
		None,
		Content,
		Block
	}
}
