---
title: Adaptive Simpson's Method
tags: Math
author: Andrew Pantuso
---

The Adaptive Simpson's Method is a method for calculating
the integral of a function on the interval \\([a,b] \\) such that
\\(a,b \\in \\mathbb{R} \\). It is derived from the closed Newton-Cotes
formulas in particular the Composite Simpson's rule.

Let \\(f \\in C\^4[a,b]\\), \\(n\\) be even, \\( h = (b-a)/n\\),
and \\( x\_j = a + jh\\), for each \\(j = 0,1,...,n\\).
There exists a \\(\\mu \\in (a,b)\\) for which the **Composite
Simpson's rule** for \\(n\\) subintervals can be written with its
error term as \\[\\int_a^b f(x) dx = h/3 \\]
