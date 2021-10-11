# Circle Packing

This MIRO App is based on the [cpack](https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_cpack.html) model from the GAMS Model library.

Given the unit circle (of radius 1), the goal is to find a set of identical size circles with an optimized (maximal) radius r so that all such circles are contained by the unit circle, in a non-overlapping arrangement.

Note that solvers will be provided with a trivial start point where the centers of all inner circles are placed on the x axis.

![](static_cpack/startpoint.png =300x300)

While local solvers may not improve the start point and declare it as local optimum, global solvers are usually able to find solutions better than the trivial start point.

![](static_cpack/optimal.png =300x300)
