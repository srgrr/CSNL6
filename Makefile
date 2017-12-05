bam: bam.cc
	g++ -o bam bam.cc -O2 -std=c++11 -Wall

bam_kprime: bam_kprime.cc
	g++ -o bam_kprime bam_kprime.cc -O2 -std=c++11 -Wall


all: bam bam_kprime
