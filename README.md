LZ4 for .NET (original C sources translated to C#)
=======================

LZ4 has been written by Yann Collet and the original sources can be found on https://github.com/lz4/lz4  
This project contains the translated C# sources

How to use
----------------------------

**non-streaming**

Non streaming version, works with managed byte arrays, uses the LZ4 Framing Format (v1.6.1).  
The compressed data that this stream creates is readable by the lz4 command line tools.  

The Compress function has additional parameters to control the result.  
 - blockMode: Linked or Independent blocks  
 - blockSize: 64KB / 256KB / 1MB / 4MB  
 - checksumMode: None / Content / Block  
 - maxFrameSize: optional maximum frame size  
 - highCompression: use lz4hc mode  

Example:  

```csharp
  // compress data
  byte[] dataToCompress = ...
  byte[] compressedData = LZ4Utility.Compress(dataToCompress);
  // decompress data
  byte[] decompressedData = LZ4Utility.Decompress(compressedData);
```  


**streaming**

Streaming version, uses the LZ4 Framing Format (v1.6.1).  
The compressed data that this stream creates is readable by the lz4 command line tools.  

Example:  

```csharp
  // compress data [with content checksum]
  using (LZ4Stream stream = LZ4Stream.CreateCompressor(innerStream, LZ4StreamMode.Write, LZ4FrameBlockMode.Linked, LZ4FrameBlockSize.Max64KB, LZ4FrameChecksumMode.Content)) {
    // write uncompressed data to the lz4 stream
	// the stream will compress the data and write it to the innerStream
	stream.Write(buffer, 0, buffer.Length);	
  }
  
  // compress data [with block and content checksum, start a new frame after 100 data blocks]
  // note: the lz4 command line tools do NOT support block checksums currently
  using (LZ4Stream stream = LZ4Stream.CreateCompressor(innerStream, LZ4StreamMode.Write, LZ4FrameBlockMode.Linked, LZ4FrameBlockSize.Max64KB, LZ4FrameChecksumMode.Block | LZ4FrameChecksumMode.Content, 100)) {
    // write uncompressed data to the lz4 stream
	// the stream will compress the data and write it to the innerStream
	stream.Write(buffer, 0, buffer.Length);	
  }
  
  // decompress data (in read mode)
  using (LZ4Stream stream = LZ4Stream.CreateDecompressor(innerStream, LZ4StreamMode.Read)) {
    // the lz4 stream will read the compressed data from the innerStream
    // and return the uncompressed data in 'buffer'
	int bytesRead = stream.Read(buffer, 0, buffer.Length);
  }
  
  // decompress data (in write mode)
  using (LZ4Stream stream = LZ4Stream.CreateDecompressor(innerStream, LZ4StreamMode.Write)) {
    // the lz4 stream will decompress the data from 'buffer'
	// and write the uncompressed data to the 'innerStream'
	stream.Write(buffer, 0, buffer.Length);
  }
```
