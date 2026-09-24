*--------------------- MODEL DESCRIPTION ---------------------------------
$ontext
* ------- DESCRIPTION OF BASIC MODEL -------------------------------------

The model is a c commodity, a activity, f factor, h household,
               e enterprise open economy model with 1 RoW account.

Versions of the model with
    i)   multiple trade partners
    ii)  nested energy inputs
    iii) nested factors of production
are available.

* ------- DATA -----------------------------------------------------------

The data are stored as a SAM. The model uses a SAM that has the following
structure of potentially active submatrices: X and Y mark submatrices active
in the database and model - submatrices marked X are strictly 0 to +inf
while sumatrices marked Y are bounded -inf to +inf.

         1      2      3       4       5      6     7      8      9
      ¦ COM  ¦ MARG ¦ ACT  ¦ FACT ¦ HHOLD ¦ ENTP ¦ GOVT ¦ KAP  ¦ ROW
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
COM   ¦  0   ¦  X   ¦  X   ¦  0   ¦   X   ¦   0  ¦   X  ¦  Y   ¦  X
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
MARG  ¦  X   ¦  0   ¦  0   ¦  0   ¦   0   ¦   0  ¦   0  ¦  0   ¦  0
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
ACT   ¦  X   ¦  0   ¦  0   ¦  0   ¦   0   ¦   0  ¦   0  ¦  0   ¦  0
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
FACT  ¦  0   ¦  0   ¦  X   ¦  0   ¦   0   ¦   0  ¦   0  ¦  0   ¦  Y
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
HHOLD ¦  0   ¦  0   ¦  0   ¦  X   ¦   Y   ¦   Y  ¦   Y  ¦  0   ¦  Y
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
ENTP  ¦  0   ¦  0   ¦  0   ¦  X   ¦   0   ¦   0  ¦   Y  ¦  0   ¦  0
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
GOVT  ¦  Y   ¦  0   ¦  Y   ¦  Y   ¦   Y   ¦   Y  ¦   0  ¦  0   ¦  Y
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
KAP   ¦  0   ¦  0   ¦  0   ¦  0   ¦   Y   ¦   Y  ¦   Y  ¦  0   ¦  Y
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------
ROW   ¦  X   ¦  0   ¦  0   ¦  Y   ¦   0   ¦   0  ¦   0  ¦  0   ¦  0
------¦------¦------¦------¦------¦-------¦------¦------¦------¦------


* #### TRADE MODELING

Trade is modelled using the Armington insight (Armington, 1969) under the
assumption that imperfect substitution between domestically produced and
imported goods is represented by a one-level CES function. In addition,
exports are assumed to be imperfect sbstitutes for domestically produced goods
and this is represented by a one-level CET function.

Small country assumption can be relaxed with Export Demand functions.

This model allows for non-traded and non-produced and non-consumed
domestic goods (ARMALT and CETALT).

* ## Imports
QQ is the quantity of a composite commodity, QM is the quantity of that
commodity imported, QD is the quantity of that commodity that is produced
and consumed domestically. CES indicates a CES function.

                                QQ
                             __/  \__
                          __/  CES   \__
                      __/               \__
                     /                      \
                    QM                      QD


* ## Exports
QXC is the quantity of a commodity produced domestically, QE is the quantity
of that commodity exported, QD is the quantity of that commodity that is
consumed domestically. CET indicates a CET function.

                                QXC
                             __/  \__
                          __/  CET   \__
                      __/               \__
                     /                      \
                    QE                      QD

* #### PRODUCTION STRUCTURE

This version uses a two-stage production structure. The top-level assumes CES
or Leontief technologies while the second-level assumes CD or CES production
functions, i.e., for a typical activity a

                                QX
                              _/  \_
                           _/CES/Leo \_
                        _/              \_
                      /                    \
                    QINT                  QVA
                    _/ \_                _/ | \_
                 _/  Leo \_            _/ CES  \_
               _/          \_        _/     |    \_
              /              \     /        |      \
           QINTD1         QINTD2  SLab    UsLab    Cap

(Leo indicates Leontief technologies, CES indicates CES function)
(QX is the output of an activity, QINT is the aggregate intermediate input,
QINTD1 and QINTD2 are representative intermediate inputs, QVA is the aggregate
quantity of value addded SLab and UsLab are Skilled and Unskilled Labour,
and Cap is Capital.

* #### HOUSEHOLD UTILITY FUNCTION

Utility functions are CD or Stone Geary functions

$offtext

*--------------------- END OF MODEL DESCRIPTION --------------------------
