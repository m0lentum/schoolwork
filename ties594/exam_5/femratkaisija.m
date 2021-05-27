% FEM-diskretointiin (paloittain lineaariset äärelliset elementi)
% perustuva ODY-ratkaisija, joka ratkaisee Poissonin tehtävän

clear all;
warning('off', 'all');
[solmut, elementit] = verkko;

% kuinka paljon tihennetään verkkoa eli kuinka monesti jaetaan jokainen
% verkon elementin särmä kahtia
tihennyskerroin=5; %kuinka paljon tihennetään, h -> h/2^tihennyskerroin 
for ii = 1:tihennyskerroin
    [solmut, elementit] = tihennys(solmut,elementit);
end
[nsolmut,m] = size(solmut);

% lokaalit matriisit muuttujaan elmatriisit esim. elmatriisit{1} on elementin 1 lokaali matriisi
% ja elmatriisit{8}(1,2) elementin 8 lokaalin matriisin 1. rivillä 2. sarakkeessa oleva alkio
[nelementit,m]   = size(elementit);
elmatriisit = cell(1,nelementit); 
for ie = 1:nelementit
    elmatriisit{ie} = zeros(3,3);
    X = solmut(elementit(ie,:)',1:2);   % elementin ie kolmen solmupisteen koordinaatit
    ala = abs(det([ones(3,1),X])/2);    % pinta-ala
    E = [X(2,:)-X(3,:); X(3,:)-X(1,:); X(1,:)-X(2,:)];  
    mat = [[E(1,:)*E(1,:)',E(1,:)*E(2,:)',E(1,:)*E(3,:)'];...
            [E(1,:)*E(2,:)',E(2,:)*E(2,:)',E(2,:)*E(3,:)'];...
            [E(1,:)*E(3,:)',E(2,:)*E(3,:)',E(3,:)*E(3,:)']];
    elmatriisit{ie} = mat/(4*ala);  
end

% kasataan globaali matriisi, tehdään ensin tyhjä harva matriisi, jossa
% sekä rivien että sarakkeiden lukumäärä on nsolmut
Amat = zeros(nsolmut,nsolmut); %sparse-funktiolla rakenne, jossa vain ei-nolla-alkiot
for ie=1:nelementit
    iglob = elementit(ie,:);
    Amat(iglob,iglob) = Amat(iglob,iglob) + elmatriisit{ie};
end

% sisäpistesolmujen (eivät ole alueen ulkoreunalla) indeksit (reunatunnus on 0 taulukossa solmut)
sisap = find(solmut(:,3)==0);
A=Amat(sisap,sisap); %matriisiin A sisäpisteitä vastaavat osat matriisista Amat

% oikean puolen vektori  
%f = rhs('source_term',N0,elements,nodes);
% kasataan vektori

bvec = zeros(nsolmut,1);
for ie = 1:nelementit 
    
% lasketaan elementin ie massakeskipisteen koordinaatit (x1,x2)
    x1=0;
    x2=0;
    for jj=1:3
    x1 = x1 + solmut(elementit(ie,jj),1);
    x2 = x2 + solmut(elementit(ie,jj),2);
    end
    x1=x1/3; 
    x2=x2/3; 
        
    X = solmut(elementit(ie,:)',1:2);   % elemntin solmujen (kärkipisteiden) 
    ala = abs(det([ones(3,1),X])/2);    % elementin pinta-ala 
    for p = 1:3     
        ii = elementit(ie,p); %elementin lokaali solmunumero p, globaali solmunumero ii
%         X(p,1) % elementin ie solmupisteen p x-koordinaatti = solmut(elementit(ie,p),1)
%         X(p,2) % elementin ie solmupisteen p y-koordinaatti = solmut(elementit(ie,p),2)
        funktio=(3*x1+x1^2)*exp(x1)*x2*(1-x2) + 2*x1*(1-x1)*exp(x1);
        bvec(ii) = bvec(ii) + funktio * ala/3; %bvec(ii+ elvektorit(funktio,X(p,1),X(p,2)) * ala/3;     
    end
end

b = bvec(sisap); %oikean puolen vektoriin b pelkästään sisäpisteitä vastaavat osat

u = A\b; %ratkaisu alueen sisäpisteissä

% Tehdään matriisi unumeerinen, jossa alkiot kaikkia solmunumeroita kohti
% (arvo nolla reunalla, jossa homogeeninen Dirichlet'n reunaehto)
unumeerinen = zeros(nsolmut,1);
unumeerinen(sisap)=u;

% interpoloidaan unumeerinen tasaväliseen hilaan ja piirretään kuva
tasavh= 1/(2^(tihennyskerroin+1));
[XI,YI] = meshgrid([min(solmut(:,1)):tasavh:max(solmut(:,1))],...
                   [min(solmut(:,2)):tasavh:max(solmut(:,2))]);
ZI      = griddata(solmut(:,1),solmut(:,2),unumeerinen,XI,YI,'linear');

%figure(1)
%surf(XI,YI,ZI);

% lasketaan tunnettu ratkaisu
x1 = solmut(:,1); % kaikkien solmupisteiden x-koordinaatit
x2 = solmut(:,2); % kaikkien solmupisteiden y-koordinaatit
utarkka=x1.*(1-x1).*exp(x1).*x2.*(1-x2);
ZI = griddata(solmut(:,1),solmut(:,2),utarkka,XI,YI,'linear');
%figure(2)
%surf(XI,YI,ZI);

% tarkkuus
virhe = abs(unumeerinen-utarkka);
maxvirhe = max(virhe)
ZI = griddata(solmut(:,1),solmut(:,2),virhe,XI,YI,'linear');
%figure(3)
%surf(XI,YI,ZI);

energianormi = sqrt((unumeerinen-utarkka)'*Amat*(unumeerinen-utarkka))


