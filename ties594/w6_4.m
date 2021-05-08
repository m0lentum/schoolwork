referenceSol = @(x,y) sin(pi*x)*sin(pi*y);

pointCounts = 2 : 2 : 40;
times = zeros(length(pointCounts), 1);
errors = zeros(length(pointCounts), 1);
for ic = 1 : length(pointCounts)
  tStart = cputime;
  [sol, err] = w6_solver(pointCounts(ic), 1);
  times(ic) = (cputime - tStart) * 1000;
  errors(ic) = err;
end

% pointCounts on pisteiden määrä yhden akselin suunnassa,
% verrataan sen sijaan laskentapisteiden kokonaismäärään
totalPointCounts = pointCounts.^2;

hold on
xlabel('N')
loglog(totalPointCounts, times, '-ob', 'DisplayName', 'aika (ms)')
loglog(totalPointCounts, totalPointCounts.^2, '--k')

loglog(totalPointCounts, errors, '-or', 'DisplayName', 'virhe')
loglog(totalPointCounts, totalPointCounts.^(-0.5), '--k')

legend

