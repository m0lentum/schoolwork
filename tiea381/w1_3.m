f = @(x) x^3 - 1002;
df = @(x) 3*x^2;

x = 10;

while true
  x = x - f(x) / df(x);
  if abs(f(x)) < 1e-6 % 6 merkitsevää numeroa
    break;
  end
end

fprintf('%6e\n', x);
