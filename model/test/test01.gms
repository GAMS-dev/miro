$title test gmswebui
set sx 'internal set' / sxElem /
    sy 'internal set implicit defined'
    hdr 'some header' / a hdr a,b hdr b,c /;
alias (sx,isx)

$onExternalInput
Set is1(is1) 'set one' / s1Elem 'elem 1-dim'/
    is2(is1,isx) 'set two' / s1Elem.sxElem 'elem 2-dim'/
    is3(is1,isx,sy<) 'set three' / s1Elem.sxElem.syElem 'elem 3-dim'/
    is20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'set twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 'elem 20-dim' /;

Singleton Set
    iss1nt(is1) / s1Elem 'elem 1-dim'/
    iss1(is1) 'singleton set one' / s1Elem 'elem 1-dim'/
    iss2(is1,sx) 'singleton set two' / s1Elem.sxElem 'elem 2-dim'/
    iss3(is1,sx,sy) 'singleton set three' / s1Elem.sxElem.syElem 'elem 3-dim'/
    iss20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'singleton set twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 'elem 20-dim' /;

* can't do pivot with sets (yet)
Parameter
    ip0 'parameter zero' / 3.5 /
    ip1(is1) 'parameter one' / s1Elem 1/
    ip2(is1,sx) 'parameter two' / s1Elem.sxElem 2/
    ip3(is1,sx,sy) 'parameter three' / s1Elem.sxElem.syElem 3/
    ip20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'parameter twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 20 /;

Table t2(is1,hdr) 'table two missing header [MIRO:pivot]'
        a b
s1Elem  1 1;

Parameter
    ipp2(is1,sx) 'parameter two [MIRO:pivot]' / s1Elem.sxElem 2/
    ipp3(is1,sx,sy) 'parameter three [MIRO:pivot]' / s1Elem.sxElem.syElem 3/
    ipp20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'parameter twenty [MIRO:pivot]' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 20 /;
$offExternalInput

display is1,is2,is3,is20,sy;

$onExternalOutput
Set os1(is1) 'set one' / s1Elem 'elem 1-dim'/
    os2(is1,isx) 'set two' / s1Elem.sxElem 'elem 2-dim'/
    os3(is1,isx,sy) 'set three' / s1Elem.sxElem.syElem 'elem 3-dim'/
    os20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'set twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 'elem 20-dim' /;
Singleton Set
    oss1(is1) 'singleton set one' / s1Elem 'elem 1-dim'/
    oss2(is1,isx) 'singleton set two' / s1Elem.sxElem 'elem 2-dim'/
    oss3(is1,isx,sy) 'singleton set three' / s1Elem.sxElem.syElem 'elem 3-dim'/
    oss20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'singleton set twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 'elem 20-dim' /;
* can't do pivot with sets (yet)
Parameter
    op0h 'parameter zero [MIRO:hidden]' / 3.5 /
    op0 'parameter zero' / 3.5 /
    op1(is1) 'parameter one' / s1Elem 1/
    op2(is1,isx) 'parameter two' / s1Elem.sxElem 2/
    op3(is1,isx,sy) 'parameter three' / s1Elem.sxElem.syElem 3/
    op20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'parameter twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 20 /;
Parameter
    opp2(is1,isx) 'parameter two [MIRO:pivot]' / s1Elem.sxElem 2/
    opp3(is1,isx,sy) 'parameter three [MIRO:pivot]' / s1Elem.sxElem.syElem 3/
    opp20(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'parameter twenty [MIRO:pivot]' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem 20 /;
Variable
    ov0 'variable zero' / L 3.5, Scale 1.6 /
    ov1(is1) 'variable one' / s1Elem.L 1/
    ov2(is1,isx) 'variable two' / s1Elem.sxElem.L 2/
    ov3(is1,isx,sy) 'variable three' / s1Elem.sxElem.syElem.L 3/
    ov19(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'variable twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.L 20 /;

equation
    oe0 'equation zero' / L 3.5 /
    oe1(is1) 'equation one' / s1Elem.L 1/
    oe2(is1,isx) 'equation two' / s1Elem.sxElem.L 2/
    oe3(is1,isx,sy) 'equation three' / s1Elem.sxElem.syElem.L 3/
    oe19(is1,isx,sy,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1,is1) 'equation twenty' / s1Elem.sxElem.syElem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.s1Elem.L 20 /;

$offExternalOutput
$set MIRO_DEBUG true
$set MIRO run
*$set MIRO build
$include miro
