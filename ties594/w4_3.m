% vakiot tehtävästä
kappa = 2;
Q = 3e-6;
rho = 3000;
cp = 730;

% parametrit ratkaisijalle
source = @(x,y) Q / (rho * cp);
diffusivity = kappa / (rho * cp);
initState = @(x,y) 280;
topDirichlet = @(x) 280;
bottomDirichlet = @(x) 1600;
spatialStepCount = 20;
endTime = 1000000;
timeStepCount = 500;

result = w4_heat(...
  source, initState, diffusivity, ...
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  1);

% piirto

x = 0 : 1 / spatialStepCount : 1;
y = 0 : 1 / spatialStepCount : 1;

figure
s = surf(x, y, result(:,:,1));

for ti = 1 : timeStepCount
  xlabel('x')
  ylabel('y')
  zlabel('u')
  set(s, 'ZData', result(:,:,ti));
  pause(0.02);
end
