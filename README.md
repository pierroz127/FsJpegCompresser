#FsJpegCompresser

This programs implements the differents steps involved in JPEG compression and decompression algorithms.
On each block of 8x8 pixels:
* Discrete cosine transform
* Quantization
In the end, values are encoded with the Huffman algorithms. 

The program takes in input a BMP file and produce a compressed binary file with an ".fsjpg" extension.
It can also decompress these binary files and produce a black and white BMP file. 

Compression:
```
.\FsJpegCompresser.exe -c "<<path of your BMP file>>"
```

Decompression:
```
.\FsJpegCompresser.exe -d "<<path of the fsjpeg file>>" 
```
