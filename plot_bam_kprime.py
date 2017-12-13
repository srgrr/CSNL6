import subprocess
import matplotlib.pyplot as plt
import sys
from math import sqrt

def f(n0, m0, t):
  return float(2.0 * m0 * t) / float(n0)


def main():
  n0, m0, T = list(map(int, sys.argv[1:4]))
  timesteps = list(map(int, sys.argv[4:]))
  tlen = len(timesteps)
  xs =  {}
  ys =  {}
  for ts in timesteps:
    xs[ts] = []
    ys[ts] = []
  lines = subprocess.check_output(
    ['./bam_kprime_no_growth', str(n0), str(m0), str(T), *sys.argv[4:]]
  ).decode('utf-8').split('\n')
  for line in lines:
    if line:
      index, timestep, degree = list(map(int, line.rstrip().split()))
      xs[index].append(timestep)
      ys[index].append(degree)
  plt.figure('Evolution of kprime')
  for (k, x) in xs.items():
    plt.plot(x, ys[k])
  plt.plot(x, [f(n0, m0, k) for k in x])
  plt.show()


if __name__ == "__main__":
  main()
