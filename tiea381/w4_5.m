f = @(x) x.^(-x);
DEGREE = 5;
POINT_COUNT = DEGREE + 1;
% (a) tasavälinen pisteistö
xs_lin = linspace(0, 3, POINT_COUNT);
% (b) tsebyshevin pisteistö
% välillä [-1, 1]
xs_cheby = arrayfun(@(i) cos((2*i+1)/(2*DEGREE+2)*pi), 0 : DEGREE);
% siirrettynä laskentavälille [0, 3]
xs_cheby = 3/2 + (3/2)*xs_cheby;

function dds = div_difs(f, xs)
  dds = f(xs);
  % luentodioissa differenssien indeksointi 
  % alkaa nollasta ja silmukka ykkösestä,
  % matlab-vektorien kanssa täytyy siirtää yhdellä
  for i = 2 : length(xs)
    for j = length(xs) : -1 : i
      dds(j) = (dds(j) - dds(j-1)) / (xs(j) - xs(j-i+1));
    end
  end
end

dds_lin = div_difs(f, xs_lin);
dds_cheby = div_difs(f, xs_cheby);

function p = eval_newton(sample_xs, dds, x)
  n = length(dds);
  p = dds(n);
  for i = n-1 : -1 : 1
    p = dds(i) + (x - sample_xs(i)) * p;
  end
end

x_p = linspace(0, 3, 50);
plot(x_p, f(x_p))
hold on
plot(x_p, arrayfun(@(x) eval_newton(xs_lin, dds_lin, x), x_p))
legend('f(x)', 'newton(x), tasavälinen')

input('press enter to view (b)');

hold off
plot(x_p, f(x_p))
hold on
plot(x_p, arrayfun(@(x) eval_newton(xs_cheby, dds_cheby, x), x_p))
legend('f(x)', 'newton(x), tsebyshev')

