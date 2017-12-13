import subprocess
import matplotlib.pyplot as plt
import sys
from math import sqrt

def f(n0, m0, t, ti):
  return float(m0) * sqrt(float(t) / float(ti))

def get_color(x):
  l = plt.rcParams['axes.prop_cycle'].by_key()['color']
  return l[ int(x) % len(l) ]


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
    ['./bam_kprime', str(n0), str(m0), str(T), *sys.argv[4:]]
  ).decode('utf-8').split('\n')
  for line in lines:
    if line:
      index, timestep, degree = list(map(int, line.rstrip().split()))
      xs[index].append(timestep)
      ys[index].append(degree)
  plt.figure('Evolution of kprime')
  for (k, x) in xs.items():
    plt.plot(x, [sqrt(k) * y for y in ys[k]])
  plt.plot(x, [1.0 * m0 * sqrt(t) for t in x])
  #for ti in xs.keys():
  #  plt.plot(x, [f(n0, m0, k, float(ti)) for k in x], color = get_color(ti*ti))
  plt.show()


if __name__ == "__main__":
  main()
