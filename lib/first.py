#!/usr/bin/env python3

# Implementation of first() from section 1.1 of Ward & Kincaid.  What
# does it do?  Plots something like sine or cosine, idk.
#
# I wrote this to check against the output of my ocaml function and I
# have to admit this was much quicker to write and, because the given
# pseudocode is in imperative style, this was much easier to
# transcribe.

from math import *

n = 10
x = 0.5
h = 1.0

# Why is range half open?  I say 11 to mean 10, bah.
for i in range(1, 11):
    h = 0.25 * h
    y = (sin(x+h) - sin(x))/h
    error = abs(cos(x)-y)
    print("{},{},{},{}".format(i,h,y,error))

