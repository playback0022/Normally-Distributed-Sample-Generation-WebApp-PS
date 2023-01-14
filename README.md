## Problem statement
We were assigned the task of developing an application illustrating the procedure 
of generating a normally distributed random variable using the acceptance-rejection
algorithm and the Box-Muller method.

## Necessary libraries and dependencies
In order for the following code snippets to run correctly, both 'Shiny' (web app
package) and 'bslib' (theme package) must be installed and included in the *app.py* file.

`
library("shiny")
library("bslib")
`

## The Acceptance-Rejection algorithm
The rejection sampling method is a technique for generating samples from a target
probability density function (PDF), by generating samples from an auxiliary pdf
and accepting or rejecting them based on the ratio of the target pdf to the 
auxiliary pdf. This method is particularly useful for generating samples from
distributions that have no closed-form solutions for generating random samples.

Intuitively, at the points at which the PDF is greater, more uniform numbers will
be accepted, since their pair y-axis uniform values have a larger 'acceptance' range.

Our application allows the users to select a number of iterations for which the
algorithm is run, so that a larger sample can be generated. A range of numbers
can also be specified and the desired mean and variance are modifiable.

Storing the pairs of coordinates, we were able to illustrate how the algorithm
works, by plotting the points and coloring them differently. The resulting plot
shows that points falling under f(x) were accepted and that they closely follow
its curve.

A histogram of the accepted numbers on the x-axis was also employed to prove that
the generated sample follows the description given by the desired PDF.

![Box-Muller Histogram Preview](https://images2.imgbox.com/40/f1/CdUL4wl6_o.png)

![Acceptance-Rejection Plot Preview](https://images2.imgbox.com/9a/6d/8lz1xuVB_o.png)


## The Box-Muller method
The Box-Muller method is a technique for generating pairs of independent, standard,
normally distributed (i.e. Gaussian) random numbers, given a pair of independent, 
uniformly distributed random numbers. This method is particularly useful for 
generating samples from normal distributions in simulations were performance is 
critical, as it is much more efficient in practice than the rejection sampling algorithm.

The end-user can choose from the same provided parameters described previously.
A histogram of the generated numbers was employed, just as for the rejection
sampling method.

![Acceptance-Rejection Histogram Preview](https://images2.imgbox.com/cc/f7/Zw9xtFK8_o.png)

## Project Contributors
* playback0022
* Iradu15
