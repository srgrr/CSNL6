#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <cstdlib>
#include <set>

std::vector< std::vector< int > > simple_cycle(int n0) {
  std::vector< std::vector< int > > g(n0);
  for(int i = 0; i < n0; ++i) {
    int j = (i + 1) % n0;
    g[i].push_back(j);
    g[j].push_back(i);
  }
  return g;
}

std::vector< std::vector< int > > random_tree(int n0) {
  std::vector< std::vector< int > > ret(n0, std::vector<  int >());
  for(int i = 1; i < n0; ++i) {
    int j = random() % i;
    ret[i].push_back(j);
    ret[j].push_back(i);
  }
  return ret;
}

std::vector< int > degree_sequence(std::vector< std::vector< int > >& g) {
  int n = int(g.size());
  std::vector< int > ret(n, 0);
  for(int i = 0; i < n; ++i) {
    ret[i] = int(g[i].size());
  }
  return ret;
}

std::vector< int > stub_vector(std::vector< std::vector< int > >& g) {
  std::vector< int > ret;
  for(auto& adjlist : g) {
    for(auto& adj : adjlist) {
      ret.push_back(adj);
    }
  }
  return ret;
}

std::vector< std::vector< int > > random_network(int n0, int m0, int T) {
  auto g = random_tree(n0);
  auto stubs = stub_vector(g);
  for(int i = 0; i < T; ++i) {
    int vertex_id = n0 + i;
    g.push_back(std::vector< int >());
    std::set< int > already_added;
    while(int(g.back().size()) < m0) {
      int u = random() % (n0 + i);
      if(already_added.count(u) == 0) {
        g[vertex_id].push_back(u);
        g[u].push_back(vertex_id);
        already_added.insert(u);
      }
    }
    for(auto x : g.back()) {
      stubs.push_back(x);
      stubs.push_back(vertex_id);
    }
  }
  return g;
}


int main(int argc, char **argv) {
  int n0 = atoi(argv[1]);
  int m0 = atoi(argv[2]);
  int T  = atoi(argv[3]);
  auto rg = random_network(n0, m0, T);
  auto degd = degree_sequence(rg);
  for(int v : degd) {
    std::cout << v << std::endl;
  }
  /*
  std::cout << "graph G {" << std::endl;
  for(int i = 0; i < int(rg.size()); ++i) {
    auto& adjlist = rg[i];
    for(auto adj : adjlist) {
     if(i < adj) {
      std::cout << "\"" << adj << "\" -- \"" << i << "\"" << std::endl;
     }
    }
  }
  std::cout << "}" << std::endl;
  */
}
