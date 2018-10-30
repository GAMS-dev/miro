$onmulti
$offmulti
$ondigit
$onuni
alias (*,webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_,webui18_,webui19_);
set webuis1_(webui0_);
set webuis2_(webui0_,webui1_);
set webuis3_(webui0_,webui1_,webui2_);
set webuis4_(webui0_,webui1_,webui2_,webui3_);
set webuis5_(webui0_,webui1_,webui2_,webui3_,webui4_);
set webuis6_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_);
set webuis7_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_);
set webuis8_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_);
set webuis9_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_);
set webuis10_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_);
set webuis11_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_);
set webuis12_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_);
set webuis13_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_);
set webuis14_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_);
set webuis15_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_);
set webuis16_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_);
set webuis17_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_);
set webuis18_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_);
set webuis19_(webui0_,webui1_,webui2_,webui3_,webui4_,webui5_,webui6_,webui7_,webui8_,webui9_,webui10_,webui11_,webui12_,webui13_,webui14_,webui15_,webui16_,webui17_,webui18_);
webuis2_(webui0_,webui1_) = sum(webui2_, businfo_Report(webui0_,webui1_,webui2_));
businfo_Report(webuis2_,bus_s) = businfo_Report(webuis2_,bus_s) + eps;
option clear=webuis2_;
webuis2_(webui0_,webui1_) = sum(webui2_, geninfo_Report(webui0_,webui1_,webui2_));
geninfo_Report(webuis2_,gen_s) = geninfo_Report(webuis2_,gen_s) + eps;
option clear=webuis2_;
webuis4_(webui0_,webui1_,webui2_,webui3_) = sum(webui4_, branchinfo_Report(webui0_,webui1_,webui2_,webui3_,webui4_));
branchinfo_Report(webuis4_,branch_s) = branchinfo_Report(webuis4_,branch_s) + eps;
option clear=webuis4_;
webuis1_(webui0_) = sum(webui1_, fuelinfo_Report(webui0_,webui1_));
fuelinfo_Report(webuis1_,fuel_s) = fuelinfo_Report(webuis1_,fuel_s) + eps;
option clear=webuis1_;
execute_unload "gmswebui.gdx";
embeddedCode Python:
pass
endEmbeddedCode
execute 'rm -f gmswebui.gdx';
