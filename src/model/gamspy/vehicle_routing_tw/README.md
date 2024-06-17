Minimize: $\sum_{i=0}^N \sum_{j=0, j\neq i}^N \sum_{k=1}^K c_{ij} \, x_{ijk}$

subject to:

$\sum_{k=1}^K \sum_{j=1}^N x_{ijk} \le K$ for $i=0$

$\sum_{j=1}^N x_{ijk} = \sum_{j=1}^N x_{ijk} \le 1$ for $i=0$ and $k \in \{ 1,...,K \}$

$\sum_{k=1}^K \sum_{j=0, j\neq i}^N x_{ijk} = 1 $ for  $i \in \{ 1,...,N \}$

$\sum_{k=1}^K \sum_{i=0, i\neq j}^N x_{ijk} = 1 $ for  $j \in \{ 1,...,N \}$

$\sum_{i=1}^N m_i \sum_{j=0, j\neq i}^N x_{ijk} \le q_k $ for  $k \in \{ 1,...,K \}$

$ t_0 = w_0 = f_0 = 0$

$\sum_{k=1}^K \sum_{i=0, i\neq j}^N x_{ijk}(t_i + d_{ij}+ f_i + w_i) \le t_j $ for  $j \in \{ 1,...,N \}$

$e_i \le (t_i +w_i) \le l_i$  for  $i \in \{ 1,...,N \}$