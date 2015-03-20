%Test using OpenDA from Java
%author: Nils van Velzen

clear

%Start with putting all OpenDA classes on the path
%Probably we can do with less, but just to be sure ;-)

myOpenDADir='/Users/nils/Devel/openda/openda_trunk/public/bin/'
javaaddpath({[myOpenDADir,'DATools_castor.jar'],        [myOpenDADir,'ganymed.jar'],               [myOpenDADir,'svnant.jar'],...
[myOpenDADir,'DUE_subset.jar'],            [myOpenDADir,'jna.jar'],...
[myOpenDADir,'Jama-1.0.2.jar'],            [myOpenDADir,'junit-3.8.1.jar'],...
[myOpenDADir,'algorithms.jar'],            [myOpenDADir,'models.jar'],                [myOpenDADir,'swing-layout-1.0.jar'],...
[myOpenDADir,'alloy.jar'],                 [myOpenDADir,'netcdf-4.1.jar'],            [myOpenDADir,'toolsUI-4.1.jar'],...
[myOpenDADir,'application.jar'],           [myOpenDADir,'observers.jar'],             [myOpenDADir,'xalan.jar'],...
[myOpenDADir,'castor-0.9.5.jar'],          [myOpenDADir,'observers_castor.jar'],...
[myOpenDADir,'commons-discovery-0.4.jar'], [myOpenDADir,'openda_core.jar'],           [myOpenDADir,'xom-1.2.6.jar'],...
[myOpenDADir,'commons-math-1.1.jar'],      [myOpenDADir,'sgt_v30.jar'],...
[myOpenDADir,'core_castor.jar'],           [myOpenDADir,'svnClientAdapter.jar']});

%Create an input file which is actually quite empty for our Lorenz model.
fileID = fopen('LorenzStochModel.xml','w');
fprintf(fileID,'<?xml version="1.0" encoding="UTF-8"?>');
fprintf(fileID,'<LorenzConfig>');
fprintf(fileID,'</LorenzConfig>');
fclose(fileID);

%Create and initialize the model factory
factory=org.openda.models.lorenz.LorenzStochModelFactory();
factory.initialize(java.io.File(pwd),['LorenzStochModel.xml']);

%Crate a single model instance (note we can create as many as we need)
m1=org.openda.matlab.Utilities.getInstance(factory,0);
                             
%Get some information from the model on timestepping
tHor   = m1.getTimeHorizon();
tstart = tHor.getBeginTime.getMJD();
tstop  = tHor.getEndTime.getMJD();
tstep  = tHor.getStepMJD();

%Now we can do a simulation. We aks the model copies of the
%state such that we can make a nice plot later
t=tstart;
XYZ=m1.getState().getValues();
T=t;
while (1)
   t=t+2*tstep;  %Note we can make bigger steps as well
   if t>tstop; break; end;
   target=org.openda.utils.Time(t);
   m1.compute(target)
   XYZ=[XYZ m1.getState().getValues()];
   T=[T t];
end

%Make some plots of the model results
figure(1);
plot3(XYZ(1,:),XYZ(2,:),XYZ(3,:))
title('Trajectory of Lorenz model in 3D');
figure(2);
plot(T,XYZ(1,:),T,XYZ(2,:),T,XYZ(3,:))

title('Trajectory of Lorenz in time');
legend('x','y','z')

