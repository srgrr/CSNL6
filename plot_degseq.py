import matplotlib.pyplot as plt
import subprocess
import sys

def main():
  n0, m0, T = sys.argv[1:]
  output = subprocess.check_output(
  ['./bam', n0, m0, T],
  ).decode('utf-8')
  degseq = map(int, output.rstrip().split())
  plt.figure('Degseq hist')
  plt.hist(degseq)
  plt.show()

if __name__ == "__main__":
  main()
