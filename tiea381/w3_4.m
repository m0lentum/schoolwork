x_0 = [0.5 0.5 0.5].';
f = @(x) [(2*x(1) - cos(x(1)) - x(2))
          (-x(1) + 2*x(2) - cos(x(2)) - x(3))
          (-x(2) + 2*x(3) - cos(x(3)))];
% jacobiaani pisteessä x_0. tämä pidetään vakiona
jac = [(2 + sin(x_0(1))) -1 0
       -1 (2 + sin(x_0(2))) -1
       0 -1 (2 + sin(x_0(3)))];

x = x_0;
while true
  f_k = f(x);
  h = jac \ (-f_k);
  x += h;
  if norm(h) <= 1e-6
    break;
  end
end

x
