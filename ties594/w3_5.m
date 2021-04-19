u = @(x) sin(x);
du = @(x) cos(x);

% etenevä differenssi
function err = fwdDiff(u, du, stepCount)
  stepLength = 2*pi / stepCount;
  x = -pi : stepLength : pi;
  uVals = u(x);
  exactDuVals = du(x);

  approxDuVals = zeros(1, length(uVals));
  for i = 1 : length(uVals) - 1
    approxDuVals(i) = (uVals(i+1) - uVals(i)) / stepLength;
  end

  % jätetään pois viimeinen piste jolle ei saada differenssiä
  approxDuVals = approxDuVals(1 : length(uVals) - 1);
  exactDuVals = exactDuVals(1 : length(uVals) - 1);

  err = max(abs(approxDuVals - exactDuVals));
end

% takeneva differenssi
function err = bwdDiff(u, du, stepCount)
  stepLength = 2*pi / stepCount;
  x = -pi : stepLength : pi;
  uVals = u(x);
  exactDuVals = du(x);

  approxDuVals = zeros(1, length(uVals));
  for i = 2 : length(uVals)
    approxDuVals(i) = (uVals(i) - uVals(i-1)) / stepLength;
  end

  approxDuVals = approxDuVals(2 : length(uVals));
  exactDuVals = exactDuVals(2 : length(uVals));

  err = max(abs(approxDuVals - exactDuVals));
end

% toisen asteen Taylor-differenssi
function err = secondOrderDiff(u, du, stepCount)
  stepLength = 2*pi / stepCount;
  x = -pi : stepLength : pi;
  uVals = u(x);
  exactDuVals = du(x);

  approxDuVals = zeros(1, length(uVals) - 2);
  for i = 1 : length(uVals) - 2
    approxDuVals(i) = (-3*uVals(i) + 4*uVals(i+1) - uVals(i+2)) / (2 * stepLength);
  end

  approxDuVals = approxDuVals(1 : length(uVals) - 2);
  exactDuVals = exactDuVals(1 : length(uVals) - 2);

  err = max(abs(approxDuVals - exactDuVals));
end

stepCounts = [5 10 20 30 50 80 100 150 200 300 500];

% maksimivirhevektorit eri differenssimenetelmille
fwdDiffErrors = zeros(length(stepCounts), 1);
bwdDiffErrors = zeros(length(stepCounts), 1);
secondOrderErrors = zeros(length(stepCounts), 1);

for i = 1 : length(stepCounts)
  fwdDiffErrors(i) = fwdDiff(u, du, stepCounts(i));
  bwdDiffErrors(i) = bwdDiff(u, du, stepCounts(i));
  secondOrderErrors(i) = secondOrderDiff(u, du, stepCounts(i));
end

hold on
grid on
loglog(stepCounts, fwdDiffErrors, '-oc')
loglog(stepCounts, bwdDiffErrors, '-ob')
loglog(stepCounts, secondOrderErrors, '-om')
% vertailuun
loglog(stepCounts, stepCounts.^(-1), '--k');
loglog(stepCounts, stepCounts.^(-2), '--k');
xlabel('N');
ylabel('virhe');

