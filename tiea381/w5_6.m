df = @(x) e^(sin(x));
f = @(x) integral(@(t) e.^(sin(t)), 0, x) - 7;

x = 10;
while true
  x_next = x - f(x) / df(x);
  diff = x_next - x;
  x = x_next;
  if abs(diff) < 1e-6 
    break;
  end
end

fprintf('%.6g\n', x);
