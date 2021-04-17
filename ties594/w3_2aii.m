% parametrit
beta = 1;
du0 = 0;
duN = exp(1) - 1/exp(1);
f = @(x) 0;

function result = solveAndPlot(beta, du0, duN, f, stepCount)
  stepLength = 1 / stepCount;
  stepInvSq = 1 / stepLength^2;
  % u:n kerroinmatriisi
  uCoefs = zeros(stepCount + 1, stepCount + 1);
  uCoefs(1, 1) = stepInvSq;
  for i = 2 : stepCount + 1
    uCoefs(i, i) = 2 * stepInvSq + beta;
    uCoefs(i, i-1) = -1 * stepInvSq;
    uCoefs(i-1, i) = -1 * stepInvSq;
  end
  uCoefs(stepCount + 1, stepCount + 1) = stepInvSq;

  % yhtalon oikea puoli
  rhs = zeros(stepCount + 1, 1);
  x = 0 : stepLength : 1;
  for i = 1 : stepCount
    rhs(i) = f(x(i));
  end
  rhs(1) = du0 / stepLength;
  rhs(stepCount + 1) = duN / stepLength;

  result = uCoefs \ rhs;

  plot(x, result, '-o')
  xlabel('x')
  ylabel('u')
  hold on
end

stepCounts = [10 20 40 80];
knownSolution = @(x) exp(-x) + exp(x);
for i = 1 : length(stepCounts)
  result = solveAndPlot(beta, du0, duN, f, stepCounts(i));

  x = 0 : 1 / stepCounts(i) : 1;
  accurateResult = zeros(stepCounts(i) + 1, 1);
  for i = 1 : stepCounts(i) + 1
    accurateResult(i) = knownSolution(x(i));
  end

  diff = result - accurateResult;
  diffNorm = norm(diff)
end

