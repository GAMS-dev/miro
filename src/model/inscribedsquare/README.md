# Inscribed Square Problem

This MIRO App is based on the [inscribedsquare](https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_inscribedsquare.html) model from the GAMS Model library.

The inscribed square problem, also known as the square peg problem or the Toeplitz' conjecture, is an unsolved question in geometry:

#### &rarr; Does every plane simple closed curve contain all four vertices of some square?

This is true if the curve is convex or piecewise smooth and in other
special cases. The problem was proposed by Otto Toeplitz in 1911.
See also <https://en.wikipedia.org/wiki/Inscribed_square_problem>

This model computes a square of maximal area for a given curve.

Specify the x and y coordinates of a closed
curve as function in variable t, where t ranges from -pi to pi.
