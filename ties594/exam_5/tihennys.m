function [uudetsolmut,uudetelementit] = tihennys(solmut,elementit)

[nsolmut,m]  = size(solmut);
[nelementit,m]   = size(elementit);

uudetsolmut = solmut;
inode = 1;
reunataulukko = zeros(0,2); 
[nreunat,m]  = size(reunataulukko);

% Jos elementin numero ie jollain reunalla (särmällä) on käyty, 
% uusi solmu on jo tehty reunan keskikohtaan.
% Jos elementin reunalla ei ole vielä käyty, uusi solmu voidaan olettaa
% alueen reunasolmuksi. Jos kyseisellä reunalla 
% käydään toistamiseen, tiedetään sen olevan sisäsolmu (tunnukseksi 0 kolmanteen 
% sarakkeeseen ko. solmun rivillä solmunumerotaulukossa "uudetsolmut").
for ie = 1:nelementit
    
  for reuna = 1:3    

    n1 = reuna;
    n2 = mod(reuna,3)+1;
    
    solmulok(reuna) = kayty(reunataulukko, elementit(ie,n1),elementit(ie,n2)); 
  
    if (solmulok(reuna) == 0)
        
        solmulok(reuna) = inode;
        inode = inode+1;
        
        reunataulukko = [reunataulukko;elementit(ie,n1),elementit(ie,n2)];                 
        uudetsolmut = [uudetsolmut;(solmut(elementit(ie,n1),1:2) + solmut(elementit(ie,n2),1:2))/2,1];
                
    else
        uudetsolmut(nsolmut+solmulok(reuna),3) = 0;
    end
    
  end 
  
  solmulok = solmulok + nsolmut;
        
  % elementti ie on jaettu neljäksi pienemmäksi (kolmio)elementiksi
  % lokaali numerointi kiertää vastapäivään
  for ii = 1:3   
      uudetelementit(4*(ie-1)+ii,1:3) = [elementit(ie,ii), solmulok(ii),solmulok(mod(ii+1,3)+1)];
  end   
  
  uudetelementit(4*ie,1:3) = solmulok(1:3);

end


function solmu = kayty(reunataulukko, solmu1,solmu2)

[nreunat,m]  = size(reunataulukko);

solmu = 0;
for i=1:nreunat
 
    if ((reunataulukko(i,1) == solmu1 & reunataulukko(i,2) == solmu2) |...
        (reunataulukko(i,1) == solmu2 & reunataulukko(i,2) == solmu1))
        solmu = i;
        return
    end

end
