%
% $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/org/costa/testData/waqua/visualize.m $
% $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
% 


SOL1=ReadWaq(['ref_',run_name1,'/SDS-csm8_rr']);
if SOL1.CONTROL_PROCESS.ikalmn == 2
   SOL1 = ReadKal(SOL1);
end

SOL2=ReadWaq(['ref_',run_name2,'/SDS-csm8_rr']);
if SOL2.CONTROL_PROCESS.ikalmn == 2
   SOL2 = ReadKal(SOL2);
end

DIFF = DiffSol(SOL1,SOL2);

opt = ShowFld;
opt.curvil = 1;
opt.field = 'sep';
opt.igain  = 1;

subplot(1,2,1);
ShowFld(SOL1,opt);
subplot(1,2,2);
ShowFld(DIFF,opt);
title([run_name1,' - ',run_name2]);
print('-depsc',['ref_',run_name1,'/Wick.eps']);


opt.field = 'up';
opt.igain  = 2;

subplot(1,2,1);
ShowFld(SOL1,opt);
subplot(1,2,2);
ShowFld(DIFF,opt);
title([run_name1,' - ',run_name2]);
print('-depsc',['ref_',run_name1,'/NorthShields.eps']);


opt.field = 'vp';
opt.igain  = 3;

subplot(1,2,1);
ShowFld(SOL1,opt);
subplot(1,2,2);
ShowFld(DIFF,opt);
title([run_name1,' - ',run_name2]);
print('-depsc',['ref_',run_name1,'/Lowestoft.eps']);


opt.field = 'wndu';
opt.igain  = 4;

subplot(1,2,1);
ShowFld(SOL1,opt);
subplot(1,2,2);
ShowFld(DIFF,opt);
title([run_name1,' - ',run_name2]);
print('-depsc',['ref_',run_name1,'/Sheerness.eps']);

opt.field = 'wndv';
opt.igain  = 4;

subplot(1,2,1);
ShowFld(SOL1,opt);
subplot(1,2,2);
ShowFld(DIFF,opt);
title([run_name1,' - ',run_name2]);
print('-depsc',['ref_',run_name1,'/Dover.eps']);




