using System;
using System.Collections.Generic;
using System.Text;

namespace IonKiwi.lz4 {
	public sealed class LZ4UserDataFrameEventArgs : EventArgs {
		private byte[] _data;
		private int _id;

		public LZ4UserDataFrameEventArgs(int id, byte[] data) {
			this._id = id;
			this._data = data;
		}

		public byte[] Data {
			get {
				return this._data;
			}
		}

		public int Id {
			get {
				return this._id;
			}
		}
	}
}
