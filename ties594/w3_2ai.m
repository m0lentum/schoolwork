% parametrit
beta = 0;
u0 = 0;
uN = 0;
f = @(x) x - x^2;

function result = solveAndPlot(beta, u0, uN, f, stepCount)
  stepLength = 1 / stepCount;
  stepInvSq = 1 / stepLength^2;
  % u:n kerroinmatriisi
  uCoefs = zeros(stepCount - 1, stepCount - 1);
  uCoefs(1, 1) = 2 * stepInvSq + beta;
  for i = 2 : stepCount - 1
    uCoefs(i, i) = 2 * stepInvSq + beta;
    uCoefs(i, i-1) = -1 * stepInvSq;
    uCoefs(i-1, i) = -1 * stepInvSq;
  end

  % yhtalon oikea puoli
  rhs = zeros(stepCount - 1, 1);
  % ei paatepisteita mukaan
  x = stepLength : stepLength : 1 - stepLength;
  for i = 1 : stepCount - 1
    rhs(i) = f(x(i));
  end
  rhs(1) = rhs(1) + u0 * stepInvSq;
  rhs(stepCount - 1) = rhs(stepCount - 1) + uN * stepInvSq;

  result = uCoefs \ rhs;
  result = [u0; result; uN];

  plot(0:stepLength:1, result, '-o')
  xlabel('x')
  ylabel('u')
  hold on
end

stepCounts = [10 20 40 80];
knownSolution = @(x) (1/12) * (x^4 - 2*x^3 + x);
for i = 1 : length(stepCounts)
  result = solveAndPlot(beta, u0, uN, f, stepCounts(i));

  x = 0 : 1 / stepCounts(i) : 1;
  accurateResult = zeros(stepCounts(i) + 1, 1);
  for i = 1 : stepCounts(i) + 1
    accurateResult(i) = knownSolution(x(i));
  end

  diff = result - accurateResult;
  diffNorm = norm(diff)
end

