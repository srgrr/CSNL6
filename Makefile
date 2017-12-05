bam: bam.cc
	g++ -o bam bam.cc -O2 -std=c++11 -Wall


all: bam
