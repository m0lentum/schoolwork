source = @(x,y) 3*sin(pi*x)*sin(pi*y);
initState = @(x,y) sin(3*pi*x)*sin(3*pi*y);
diffusivity = 0.2;
topDirichlet = @(x) 0.5*sin(pi*x);
bottomDirichlet = @(x) -0.5*sin(pi*x);
endTime = 0.5;
timeStepCount = 200;

% ajan mittaukset prosessoriaikana

spatialStepCount = 10;

tStart = cputime;
explicitResult = w4_heat(...
  source, initState, diffusivity, ...
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  0);
tExplicit10 = cputime - tStart

tStart = cputime;
implicitResult = w4_heat(...
  source, initState, diffusivity, ...
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  1);
tImplicit10 = cputime - tStart

spatialStepCount = 20;

tStart = cputime;
explicitResult = w4_heat(...
  source, initState, diffusivity, ...
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  0);
tExplicit20 = cputime - tStart

tStart = cputime;
implicitResult = w4_heat(...
  source, initState, diffusivity, ...
  topDirichlet, bottomDirichlet, ...
  spatialStepCount, endTime, timeStepCount, ...
  1);
tImplicit20 = cputime - tStart

% implisiittisen ratkaisun piirto animoituna

x = 0 : 1 / spatialStepCount : 1;
y = 0 : 1 / spatialStepCount : 1;

figure
s = surf(x, y, implicitResult(:,:,1));

for ti = 1 : timeStepCount
  xlabel('x')
  ylabel('y')
  zlabel('u')
  set(s, 'ZData', implicitResult(:,:,ti));
  pause(0.1);
end

