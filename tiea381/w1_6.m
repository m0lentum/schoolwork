xtof = @(x) 1 / x^2;
g = @(x, re) -2 * log10(2.51 * x / re);

re = logspace(3, 7);

f = zeros(length(re), 1);

for re_i = 1 : length(re)
  % kiintopistemenetelmällä x tälle Reynoldsin luvulle
  x = 0.01;
  while true
    x_next = g(x, re(re_i));
    diff = x_next - x;
    x = x_next;
    if abs(diff) < 1e-6
      break;
    end
  end
  % vastaukseen f
  f(re_i) = xtof(x);
end

% piirto

semilogx(re, f)
xlabel('Re')
ylabel('f')
grid on
