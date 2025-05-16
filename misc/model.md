## Motivation

Real-world string data—like names—often include both **true entries** and **garbled variants** due to typos, transcription errors, or OCR mistakes. Our goal is to infer:

1. Which strings genuinely belong to the “real” set versus noise (\`x\_i\`).
2. The underlying frequency distribution over the real strings (\`p\`).
3. The error process that transforms real strings into garbled observations.

We use a **spike-and-slab** prior on inclusion (\$x\_i\$) so that most candidates are excluded, a **Dirichlet–Multinomial** for counts, and a **kernel** \$G\_{ij}(\lambda)\propto e^{-d\_{ij}/\lambda}\$ to model how likely string \$j\$ was garbled into \$i\$.  This setup balances flexibility (capturing arbitrary error patterns) with sparsity (only a few strings are truly active).

## Generative Model

Let \$K\$ be the number of candidate strings, and \$n\_j\$ the observed count of string \$j\$.  Define:

1. **Inclusion prior**:

   $$
   q_i \sim \mathrm{Beta}(\alpha^{(x)},\,\beta^{(x)}),
   \quad x_i \sim \mathrm{Bernoulli}(q_i).
   $$
2. **Mixture weights** (only over active \$x\_i=1\$):

   $$
   \alpha_i = \alpha^{(p)}\,x_i + \varepsilon,
   \quad p \sim \mathrm{Dirichlet}(\alpha_1,\dots,\alpha_K).
   $$
3. **Error kernel**: for observed string \$j\$ and true string \$i\$,

   $$
   G_{ij}(\lambda) \;=\; \frac{e^{-d_{ij}/\lambda}}{\sum_i e^{-d_{ij}/\lambda}}.
   $$
4. **Allocation of counts**: each observed \$j\$ distributes its \$n\_j\$ counts via

   $$
   (Z_{1j},\dots,Z_{Kj}) \sim \mathrm{Multinomial}\Bigl(n_j,\;\psi_{\cdot j}\Bigr),
   \quad
   \psi_{ij} \propto\begin{cases}
     (1-\delta)\,p_j, & i=j,\\
     \delta\,p_i\,G_{ij}(\lambda), & i\neq j.
   \end{cases}
   $$

The **observed total** for string \$i\$ is then \$Y\_i=\sum\_j Z\_{ij}\$.  A Beta prior on the overall error rate \$\delta\$ completes the model.

## Gibbs Sampling Algorithm

We draw from the posterior \$P(p,x,\delta \mid Y)\$ by iterating:

1. **Allocate counts** \$Z \mid p,x,\delta,\lambda\$:

   * For each \$j\$, sample a Multinomial for its \$n\_j\$ counts with probabilities \$\psi\_{\cdot j}\$.  We implement this in C++ for speed.

2. **Update mixture weights** \$p \mid Z,x\$:

   $$
   p \sim \mathrm{Dirichlet}(\alpha^{(p)} x + \varepsilon + T),
   \quad T_i=\sum_j Z_{ij}.
   $$

3. **Update inclusion indicators** \$x\_i\mid Z\$:

   * Compute the log‐Bayes‐factor comparing marginal likelihoods under \$x\_i=1\$ vs. 0,
   * Add the prior log‐odds \$\log(\alpha^{(x)}/\beta^{(x)})\$, then draw Bernoulli.

4. **Update error rate** \$\delta\mid Z\$:

   * Let \$S=\sum\_i Z\_{ii}\$ (self‐allocations) and \$E=\sum\_{i\neq j}Z\_{ij}\$;
   * Sample \$\delta\sim\mathrm{Beta}(\alpha^{(\delta)}+E,,\beta^{(\delta)}+S).

5. **(Optional) Summaries**: after burn‐in, accumulate posterior means of \$x\$, \$p\$, and \$\delta\$.

## Tradeoffs and Extensions

* **Motivation for spike-and-slab**: by letting most \$x\_i=0\$, we avoid overfitting noise strings and focus mass on a small active set.
* **Choice of \$\lambda\$**: smaller \$\lambda\$ localizes the error kernel (only near neighbors), while larger \$\lambda\$ allows long‐range garbles; learning \$\lambda\$ adds complexity, so it can be fixed or updated via Metropolis–Hastings.
* **Sparsity vs. bias**: extreme sparsity can miss rare but real strings; relaxing prior hyperparameters trades off false positives vs. false negatives.
* **Scalability**: C++ allocations and sparse neighbor lists handle large \$K\$ (e.g. millions) but may require intelligent blocking or approximate nearest‐neighbor methods in practice.

This framework is flexible: you can swap in different string‐distance metrics, hierarchical priors, or approximate inference (e.g. variational) to suit your data and scale requirements.
