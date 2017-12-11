bam: bam.cc
	g++ -o bam bam.cc -O2 -std=c++11 -Wall

bam_kprime: bam_kprime.cc
	g++ -o bam_kprime bam_kprime.cc -O2 -std=c++11 -Wall

bam_random: bam_random.cc
	g++ -o bam_random bam_random.cc -O2 -std=c++11 -Wall

bam_kprime_random: bam_kprime_random.cc
	g++ -o bam_kprime_random bam_kprime_random.cc -O2 -std=c++11 -Wall





all: bam bam_kprime bam_random bam_kprime_random
