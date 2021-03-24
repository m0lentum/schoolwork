x = dlmread('aika.txt');
y = dlmread('korkeus.txt');

plot(x, y);

i=1
while i < length(y)
  % tama on eteneva differenssi
  % (skandit ei toimi Octaven pdf-publish:ssa)
  muutosnopeus=(y(i+1)-y(i))/(x(i+1)-x(i))
  i=i+1
end
