f = @(x) x^3 - 1002;
df = @(x) 3*x^2;

x = 10;

while true
  x_next = x - f(x) / df(x);
  diff = x_next - x;
  x = x_next;
  if abs(diff) < 1e-6 
    % 6 desimaalin tarkkuus saavutettu,
    % siis vähintään 6 merkitsevää numeroa
    break;
  end
end

fprintf('%.6g\n', x);
