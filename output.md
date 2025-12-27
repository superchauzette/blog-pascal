**** ***Réalisation d'un applicatif de Musique à partir d'un logiciel
gratuit***

**s*****ans ajout de bibliothèques***

**Utilisation de Lazarus**

Quelles questions se poser avant de commencer la programmation**:**

**-**Il faut organiser les informations des fichiers de musique et la
plus adéquate est une représentation

hierachisée en arbre:

Artistes → Albums → Titres

Titre 1.mp3

Titre n.mp3

Cover.jpg

TagsParam.txt

Le volume de donnés peut être important surtout dans le cas ou les
fichiers sont codés en Hires

Ce qui peut impliqué de stocker les données sur un disque externe il
faudra donc paramétrer ces répertoires

Les diversions solutions à réaliser dépendent de :

Comment sont organisées les inter-actions entre Artistes-Album

Comment sont affichés les informations Artistes Albums

Comment lire et décoder les musiques

***les Inter-Actions***

****différentes possibilités : un fichier Text liant un artiste et ses
albums

un fichier Json contenant l'entièreté des relations Artiste-Album-Titre

une Base de données en Mémoire

une Base de données SQL

Inconvénient la mise à jour de ces fichiers pour avoir un logiciel
actualisé.

Avantage un traitement plus simple du logiciel car les relations Artiste
Albums sont déjà

faites par la création des ces fichiers et possibilités de faire des
recherches

le Balayage des répertoires permet au logiciel de répondre à tout

nouvel Album

Le décodage des fichiers de musique nécessitant une bibliothèque
spécialisée pour chaque type

mp3,wav ,Flac etc la solution retenue est reporter sur un fichier
externe

*Fichier paramétrage T**agsparam.txt***

**** il est situé dans le répertoire Album

description :

Année → 1998

Nombre de Titres → 7

soit soit soit

Titre 1 → Titre1mp3 Titre1.flac

Titre 2 → Titre2.mp3 Titre2.flac

Titre 3 → Titre3.mp3 Titre3.flac

Titre 4 → Titre4.mp3 Titre4.flac

Titre 5 → Titre5mp3 Titre5.flac

Titre 6 → Titre6.mp3 Titre6.flac

Titre 7 → Titre7.mp3 Titre7.flac

*Paramètrage de l'applicatif*

Répertoire MusiqueParam → Albums Contient les fichiers NomArtiste.txt
Relation Artiste -Album

→ Images Contient les diverses images de l'applicatif

→ Json Contient le fichier Json décrivant l'ensemble de la musique

→ Music Contient la Base SQLite et la DLL sqlite3.dll

→ PlayListe Contient les PlayListes

→ TableMem Contient le fichier alimentant la Table en mémoire

**Définir l'Interface Homme machine**

L'écran est divisé en trois zones

Zone Artistes Sélection d'un artiste

Zone Albums Sélection d'un album

Zone Titres Sélection d'un titre

***Ecran de l'applicatif***

**Les différentes Versions**

**Les informations pour les Artistes sont affichées**

*soit sous forme de liste → ListBox*

*soit sous forme d'arbre relation artistes-albums → Treeview*

**Les informations pour les Albums sont affichées**

****

**** *****par les pochettes des albums d'un artiste dans une scrollbox*

*****par un écran comportant les pochettes de plusieurs artistes*

*suivant le nombre d'album dans un panel gérer par une pagination *****

***Les informations pour les titres sont affichées***

**

*sous forme de liste listBox*

**4 possiblités pour IU** ListBox (Artistes) +ScrollBox (Albums)

ListBox (Artistes) +Panel (Albums)

TreeView (Artistes-Album) +ScrollBox (Albums)

*TreeView (Artistes-Album) +Panel (AlbumsUtilisation de fichier*

***Structure du fichier Text** relation Artiste → Album*

**

*Artiste1.txt Album1*

Annee 1

Album 2

Annee 2

Album n

*Annee n*

On a donc 4x4 =16 possibillités

Fichier Text

******Fichier Text Artiste.txt avec ListBox Artiste scrollbox Album

******Fichier Text Artiste.txt avec ListBox Artiste panel Album

Fichier Text Artiste.txt avec TreeView Artiste-Album scrollbox Album

Fichier Text Artiste.txt avec TreeViewArtiste-Album panel Album

Fichier JSon

******

Fichier Json avec ListBox Artiste scrollbox Album

******Fichier Json avec ListBox Artiste panel Album

Fichier Json Artiste.txt avec TreeView Artiste-Album scrollbox Album

Fichier Json Artiste.txt avec TreeViewArtiste-Album panel Album

*Fichier TableMem*

Fichier TableMem avec ListBox Artiste scrollbox Album

Fichier TableMem avec ListBox Artiste panel Album

Fichier TableMem Artiste.txt avec TreeView Artiste-Album scrollbox Album

*Fichier TableMem Artiste.txt avec TreeViewArtiste-Album panel Album*

BD SQLite

Fichier BD SQLite avec ListBox Artiste scrollbox Album

Fichier BD SQLite avec ListBox Artiste panel Album

Fichier BD SQLite Artiste.txt avec TreeView Artiste-Album scrollbox
Album

Fichier BD SQLite Artiste.txt avec TreeViewArtiste-Album panel Album

**Répertoire**

Fichier Répertoire avec ListBox Artiste scrollbox Album

Fichier Répertoire avec ListBox Artiste panel Album

Fichier Répertoire Artiste.txt avec TreeView Artiste-Album scrollbox
Album

Fichier Répertoire Artiste.txt avec TreeViewArtiste-Album panel Album

**Construction du logiciel**

****Première étape Utilsation de UI définie au préalable en trois zone,

codage de la partie artiste

codage de la partie Titres +,Lecteur Audio

la partie de gestion des albums est minimaliste au départ pour réaliser

toutes les procédures communes à toutes les versions et éviter de faire

du code non réutilisable.

Ci dessous l'écran de départ pour réaliser les procédures communes à
toutes les versions

le code pour alimenter La Filebox Album

SDir:=CheminMorceaux+ArtisteSelect;

FileListBoxAlbum.Directory:=SDir;

<img src="Pictures/1000020100000777000003F9A26C25B97F37F20D.png"
style="width:6.6925in;height:3.561in" />

on a: Panel Artistes avec une ListBoxArtistes

Panel Titres avec une LisBoxTitres

Panel Albums avec une FileBox en version provisoire

Une toolBar avec des speedBoutons porur chaque lettres permetant de se
positionner

sur un artiste et un bouton info donnant le nombre d'artiste et d'albums

des étiquettes pour visualiser des informations sur artiste,album et
titres ainsi qu'une image de la pochette de l'album sélectionné et celui
qui est joué et des boutons pour gerer le lecteur

La procédure GetDrive définit les variables ChParam MusiqueParam

Sdrive d:\\

CheminMorceaux; d:\Music\\

CheminPlayListe; d:\MusicParam\PlayListe\\

Le drive est déterminé en cherchant le fichier UnHommeAverti.txt dans le
répertoire QuelDrive

Affichage des artistes

Artistes:TStringList;

ListListBoxArtistes.Items.Clear;

Artistes := FindAllDirectories(CheminMorceaux,false);

Artistes.sort;

for i:=0 to Artistes.Count -1 do
ListBoxArtistes.Items.Add(ExtractFileName(Artistes\[i\])) ;

Par la procédure ListBoxArtistesClick on alimente la box Albums

par la procédure ListBoxAlbumsClick on alimente la ListBoxTitres

par la procédure ListBoxTitresClick on enclenche le lecteur audio et
l'affichage des informations

de l'album joué artiste album et titre joué

par la procédure ListBoxTitresDblClick on enclenche la lecture de
l'album en entier

Le lecteur audio utilisé est le Window média player sous forme OleObject

WMP: OleVariant;

if Assigned(InitProc) then TProcedure(InitProc);

try

WMP := CreateOleObject(ServerName);

except

showMessage('Unable to start WMP');

Exit;

end;

les fonctions disponibles sont la lecture d'un titre WMP.url:= ← Chanson
WMP.controls.play;

l'arrêt d'un titre WMP.Controls.Stop;

WMP.controls.Pause

WMP.PlayState

(VarToStr(WMP.PlayState)='1') Le titre joué est terminé

On utilise un timer pour gérer l'affichage des titres dans le cas de la
lecture d'un album ou d'une

playlistes en synchronisant un tableau dynamique PL avec la lecture des
titres

**Gestion des playListes**

Panel PL →GrillePL

S

La grille des playListe est alimentée soit par la ListBoxTitres pour une
création

soit par FileListBoxPL pour un fichier existant

par LitNewPlayListe(NamePlayListe)

Glisser déposer Drag and Drop pour alimenter la Listbox Grille des
PlayListes

évenements: GrillePLDragDrop, GrillePLDragOver, GrillePLDrawItem

Le panel GrillePL contient une Flistbox pour les playlistes

une Listbox pour les titres de la playlistes

un bouton pour supprimer une ligne de titre de la playliste

un bouton pour vider la liste des titres de la play Liste

un bouton pour sauvegarder une play liste

un bouton pour supprimer une Play liste

une zone de saisie du nom de la play liste

Les ListboxTitres et GrillePL doivent être mis en **dragMode**
dmAutomatic

et la GrillePL **Style** lbOwnerDrawFixed pour pouvoir utiliser
OnDrawItem

afin d'utiser plusieur colonnes avec une synchronisation Tableau PL et
le contenu

de la GrillePL

La grille PL est une ListBox et la mise en forme de l'affichage est
réalisée par

procedure TForm1.**GrillePLDrawItem(**Control: TWinControl; Index:
Integer;

ARect: TRect; State: TOwnerDrawState);

begin

with GrillePL.Canvas do

begin

FillRect(ARect);

if GrillePL.Selected\[Index\] then Font.Color:=clwhite

else Font.Color:=clblack;

TextOut(ARect.Left, ARect.Top, Copy(PL\[Index\].Artiste,1,26));

TextOut(ARect.Left+250,ARect.Top, Copy(PL\[Index\].Album,1,30));

TextOut(ARect.Left+560,ARect.Top,Copy(PL\[Index\].Titre,1,30)); //n-1));

end;

end; // GrillePLDrawItem

11 Procedures utiliséés pour Gerer les PlayListes

GrillePLDragDrop(Sender, Source: TObject; X, Y: Integer); Réalise le
transfert de la ListboxTitre

GrillePLDragOver(Sender, Source: TObject; X, Y: Integer;

FaitFichierWPL(NomPList:string); Mise en forme du fichier WPL

BtSauvePLClick(Sender: TObject); Sauvegarde de la grillePL

BtSupPLClick(Sender: TObject); Suprime une PlayLise

BtVideGrilleClick(Sender: TObject); Remet la grille à vide

JouePL; Soumet la PlayListe au lecteur audio

BtJouePLClick(Sender: TObject); Démarrage de la PlayListe

llitNewPlay(NamePlay:string); Décode le fichier W

Ceci termine la partie commune Phase 1-1 & Phase 1-2

***Phase 2***

Version avec fichier Text avec affichage pochettes d'un artiste
sélectionné dans une scrollbox

***Structure du fichier Text** relation Artiste → Album*

*Artiste1.txt Album1*

Annee 1

Album 2

Annee 2

Album n

*Annee n*

*decodage du fichier ArtisteSel.txt*

*procedure TForm1.Albums_Artiste(ArtSel:string);*

*var*

*SDir:string;*

*i:integer;*

*TL: TStringList;*

*begin*

*TL:=TStringList.Create;*

*SDir:=SDrive+ChParam+'\Albums\\+ArtSel+'.txt';*

*TL.LoadFromFile(SDir);*

*for i:=0 to TL.Count-1 do*

*begin*

*if i mod 2=0 then*

*begin*

*SetLength(Albums,TotalAlbum+1);*

*Albums\[TotalAlbum\].NomAlbum:=TL\[i\];*

*Albums\[TotalAlbum\].NomArtiste:=ArtSel;*

*end*

*else*

*begin*

*Albums\[TotalAlbum\].Annee:=StrToInt(TL\[i\]);*

*inc(TotalAlbum);// 2eme Ligne Année Album*

*end;*

*end;*

*TL.Destroy;*

*end; // Albums_Artiste*

*Dimenzion=record Const Dimension1:Dimenzion=(*

*DimImage:integer; DimImage:180*

*MargeLeft:integer; MargeLeft:210;*

*MargeTop:integer; MargeTop:15;*

*MargeInter:integer; MargeInter:20;*

*MargeTitre:integer; MargeTitre:8;*

*MargeImage:integer; MargeImage:22;*

*HtTitreAlb:integer; HtTitreAlb:16;*

*HtEtiqYear:integer; HtEtiqYear:10;*

*end;*

***Positionnement dans la scrollBox***

**

*Marge Top*

*DimImage*

*MargeInter*

*MargeLeft*

*MargeImage*

**

*HtTitreAlb*

*MargeTitre*

*HtEtiqYear*

*A**ffichage des pochettes***

*procedure TForm1.Creation_ImageEtiquettes(Dim:Dimenzion);*

*var*

*Col,PasX,i:integer;*

*SImage:string;*

*comp: TComponent;*

*UneImage:TImage;*

*UneEtiquette:TLabel;*

*begin*

*ListBoxtitres.Items.clear;*

*for i:=Form1.ComponentCount-1 downto 0 do*

*begin*

*If Components\[i\] Is TImage then*

*begin*

*UneImage:= Components\[i\]as Timage; Suppresion des images et
étiquettes*

*Simage:=Copy(UneImage.name,1,5); Clear la scrollBox Album*

*if SImage= 'Image' then*

*Components\[i\].Destroy;*

*end;*

*comp:=FindComponent('Etiq'+intToStr(i));*

*if comp is TLabel then comp.free;*

*comp:=FindComponent('EtiqY'+intToStr(i));*

*if comp is TLabel then comp.free;*

*end;*

*PasX:=Dim.DimImage+Dim.MargeInter; Création de s étiquettes et de
l'image de la*

*for Col:=0 to TotalAlbum-1 do pochette*

*begin*

*UneImage:=TImage.Create(Self);*

*with UneImage do*

*begin*

*Parent :=ScrollBoxAlbums;*

*height:=Dim.DimImage;*

*width:=Dim.DimImage;*

*top:=Dim.MargeTop;*

*left:=Dim.MargeLeft+(Col-1)\*PasX;*

*stretch:=true;*

*SImage:=CheminMorceaux+ArtisteSelect+'\\+Albums\[Col\].NomAlbum+'/cover.jpg';*

*picture.LoadFromFile(SImage);*

*OnClick:=@Form1.ClickImage;*

*name:='Image'+intTostr(Col);*

*visible:=true;*

*Hint:='Cliquer pour obtenir les titres';*

*ShowHint:=True;*

*Tag:=Col;*

*end;*

*UneEtiquette:=TLabel.Create(Self);*

*with UneEtiquette do*

*begin*

*parent:=ScrollBoxAlbums;*

*top:=Dim.MargeTop+Dim.DimImage;*

*left:=10+Dim.MargeLeft+(Col-1)\*PasX;*

*Font.size:=12 ; //Dim.HtTitreAlb DIV 2;*

*Font.Color:=clblack;*

*Font.Style:=\[fsbold\];*

*name:='Etiq'+intTostr(Col);*

*Caption:=Albums\[Col\].NomAlbum;*

*end;*

*UneEtiquette:=TLabel.Create(Self);*

*with UneEtiquette do*

*begin*

*parent:=ScrollBoxAlbums;*

*top:=10+Dim.MargeTop+Dim.DimImage+Dim.HtTitreAlb;*

*left:=10+Dim.MargeLeft+(Col-1)\*PasX;*

*Font.size:=Dim.HtTitreAlb;*

*Font.Color:=clblack;*

*Font.Style:=\[fsbold\];*

*Font.size:=10;*

*name:='EtiqY'+intTostr(Col);*

*Caption:=IntToStr(Albums\[Col\].Annee);*

*end;*

*end;// for*

*end; // Creation_ImageEtiquettes*

**Clique sur une image**

procedure TForm1.ClickImage(Sender: TObject);

var

Tg,i,NbTitres:integer;

Image:TImage;

SImage,NameFich,LigUTF8:string;

TL:TStringList;

begin

Image:=Sender as TImage;

Tg:=Image.Tag;

SImage:=CheminMorceaux+ArtisteSelect+'\\+Albums\[Tg\].NomAlbum+'\cover.jpg';

CoverSel.picture.loadfromfile(SImage);

CoverSel.visible:=true;

**** LblSelAlbum.visible:=true;

LblSelArtiste.visible:=True;

LblSelAlbum.caption:=' '+Albums\[Tg\].NomAlbum+' ';

LblSelArtiste.Caption:=' '+ArtisteSelect+' ';

ListBoxTitres.Items.Clear;

AlbumSelect:=Albums\[Tg\].NomAlbum;

NameFich:=CheminMorceaux+ArtisteSelect+'/'+Albums\[Tg\].NomAlbum+'/'+'TagsParam.txt';

TL:=TStringList.Create;

if FileExists(NameFich) then

begin

TL.LoadFromFile(NameFich); TL\[0\] Contient l'Année

TL.Delete(0);

NbTitres:=StrToInt(TL\[0\]); TL\[0\] Contient le nombre de titres

TL.Delete(0);

For i:=0 to NbTitres-1 do

begin

LigUTF8:=ISO_8859_1ToUTF8(TL\[i\]);

ListBoxTitres.Items.Add(LigUTF8);

end;

end;

end; // Tform1.ClickImage

*Même Version mais avec un arbre TreeView*

** on construit un tableau des albums du fait que l'on doit reproduire
de façon graphique la structure Artistes → Albums par le code inclus au
démarrage → FormCreate

TotalAlbum:=0;

for i:=0 to Artistes.Count-1 do
Albums_Artiste(ExtractFileName(Artistes\[i\]));

Type var

AlbTyp=record Albums:array of AlbTyp;

NomArtiste:string;

NomAlbum:string;

Annee:integer;

end;

procedure TForm1.Albums_Artiste(ArtSel:string);

var

SDir:string;

i:integer;

TL: TStringList;

begin

TL:=TStringList.Create;

SDir:=SDrive+ChParam+'\Albums\\+ArtSel+'.txt';

TL.LoadFromFile(SDir);

for i:=0 to TL.Count-1 do

begin

if i mod 2=0 then

begin

SetLength(Albums,TotalAlbum+1); // Nom Album

Albums\[TotalAlbum\].NomAlbum:=TL\[i\];

Albums\[TotalAlbum\].NomArtiste:=ArtSel;

end

else

begin

Albums\[TotalAlbum\].Annee:=StrToInt(TL\[i\]);

inc(TotalAlbum);// 2eme Ligne Année Album

end;

end;

TL.Destroy;

end; // Albums_Artiste

*Code pour construire le TreeView*

Boucle sur les Artistes

Pour un ArtisteX

Tview.**Items.Add**(RootNode,Art);

boucle sur les Albums de l'Artiste X

Tview.Items.**AddChild**(Node,AlbTr\[i\].TitreAlb);

For i:=0 to TotalAlbum-1 do

begin

if Albums\[i\].NomArtiste\<\>MemoArtiste then

begin

Node:=Tview.Items.Add(RootNode,Albums\[i\].NomArtiste);

Tview.Items.AddChild(Node,Albums\[i\].NomAlbum);

MemoArtiste:=Albums\[i\].NomArtiste;

end

else Tview.Items.AddChild(Node,Albums\[i\].NomAlbum);

end;

Les Versions suivantes utilise un **système de pagination**

Ecran avec le système de pagination

Intterface avec Pagination

**Affichage des Albums avec Pagination**

MargeTop MargeTop MargeTop

HtEtiqArt

MargeImage MargeImage

MargeLeft DimImage

MargeInter MargeInter

MargeTitre

HtEiqAlb

HtEtiqYear

MargeInterOp

HtEtiqArt

MargeImage

MargeLeft MargeInter MargeInter

DimImage

PasX:=Dim.DimImage+Dim.MargeInter;

Une étiquette Artiste est inséré à chaque fois que l'on change d'artiste

Le pas variable → + HtEtiqArt+MargeImage est ajouté à chaque changement
d'artiste

**PasFixe:=MargeInterTop+DimImage+MargeTitre++HtTitreAlb+HtEtiqYear**;

**PasVariable:=PasVariable+Dim.HEtiqArtiste+Dim.MargeImage;**

**Position Y MargeTop+PasVariable+(PasFixe\*(Ligne-1));**

**PasFixe:=MargeTop+DimImage+MargeTitre++HtTitreAlb+HtEtiqYear**

5 130 8 16 10 **169**

**PasVariable:=PasVariable+Dim.HEtiqArtiste+Dim.MargeImage;**

11 22 **33**

EtiqArtiste\[\].top:= Dim.MargeTop+PasVariable+(PasFixe\*(NoImage-1));

ImageAlb\[\].top:=PosImage;

EtiqAlb\[.top:=PosImage+Dim.DimImage+Dim.MargeTitre;

EtiqYear\[\].top:=PosImage+Dim.DimImage+Dim.MargeTitre+Dim.HtTitreAlb;

**PosImage:=Dim.MargeTop+PasVariable+(PasFixe\*(Ligne-1));**

Le calcul du nombre nécessaire de ligne pour afficher tous les albums
d'un artiste

est réalisé par LigneComplete= NombreAlbum DIV NbImagesparLigne

Ligne Incomplete = NombreAlbum MOD NbImagesparLigne

**Modulo 7 (** ***7 images par Ligne*** **)**

N 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18

DIV 0 0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 2 2 2

MOD 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4

**Modulo 8 ( 8** ***images par Ligne*** **)**

N 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18

DIV 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2

MOD 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7 0 1 2

**Modulo 5 ( 5** ***images par Ligne*** **)**

N 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18

DIV 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3

MOD 0 1 2 3 4 0 1 2 3 4 0 1 2 3 4 0 1 2 3

Ex : Artiste → 17 Albums pour 7 Albums par Ligne

Ligne Complete = 2

Ligne Incomplete 3 3 albums sur cette ligne

On donc 3 Lignes pour afficher 17 Albums avec 7 albums par ligne

pour 5 Albums par Ligne

Ligne Complete = 3

Ligne Incomplete 2 2 albums sur cette ligne

**Systeme de pagination**

Preparation à la Pagination

On construit un tableau dynamique contenant tous les albums contenant
les informations

**NomArtiste,NomAlbum:,Annee,NbAlbumArtiste,NoOrdreAlbCetArtiste**

****

**p**ar la Procedure FaitAlbums(Artistes) →Albums\[\]

N° : 1 Artiste : Artiste1 Album : Album1 année : Année1 Nbre Album : Nb1
N° Alb : Ordre1

N° : 2 Artiste : Artiste1 Album : Album2 année : Année2 Nbre Album : Nb1
N° Alb : Ordre2

N° : 3 Artiste : Artiste2 Album : Album3 année : Année3 Nbre Album : Nb2
N° Alb : Ordre1

****CreationEtiquetteImage crétion des étiquettes et des images

Calcul du nombre de lignes nécessaire pour afficher sur un écran infini
tous les albums

suivant une matrice de NbImgParLigne\*NbreLigneByEcran

**CalculNbrePage** Nombres de lignes pour afficher tous les albums -

Calcul pour un artiste donné en fonction du nombre d'album de l'artiste

Nombre de Ligne completes NbreAlbum DIV NbImgParLigne

Nombre d'albums sur la Ligne incomplete NbreAlbum MOD NbImgParLigne

CptLigne :Nombre de lignes pour afficher l'ensemble des albums

CptLigne= Nombre de Ligne complètes

Si Nombre d'albums sur la Ligne incomplète \<\>0 CptLigne+1

Pour tous les artistes on obtient le total des lignes pour afficher
l'ensemble des albums

En décomposant par pages ayant NbreLigneByEcran de lignes sur l'ecran

et comme la page n reprend la ligne 2 de la page précédente en Ligne 1

la ligne NbreLigneByEcran de la page précédente sur sur la ligne
NbreLigneByEcran-1

sur la Page 0 on donc NbreLigneByEcran Lignes et sur les pages suivantes
on ajoute

une ligne

on a donc pour afficher l'ensemble des albums

Si CptLigne \< NbreLigneByEcran+1 → On a **1 page** → Page\[0\]

Sinon Nombre de page = **CompteurLigne-NbreLigneByEcran+1**;

**Structure des pages & Remplissage par les deux procedures ci dessous**

NoDisque_Debutant_Ligne:array\[1..NbreLigneByEcran \] of integer;

NbreAlbum:array\[1..NbreLigneByEcran \] of integer;

**PrepPage(NoAlbDebutPage:integer;NumPage:integer;NbImgParLigne:integer);**

**PrepareLig(NumAlbum:integer;NoLigne:integer;NoPage:integer);**

Ex Tableau Page

N Page 0

Rang 1 Nombre Album 1 Debutant N 0 Album1 Artiste1

Rang 2 Nombre Album 2 Debutant N 1 Album2 Artiste2

Rang 3 Nombre Album 1 Debutant N 3 Album3 Artiste3

Rang 4 Nombre Album 1 Debutant N 4 Album4 Artiste4

N Page 1

Rang 1 Nombre Album 2 Debutant N 1 Album2 Artiste2

Rang 2 Nombre Album 1 Debutant N 3 Album3 Artiste3

Rang 3 Nombre Album 7 Debutant N 4 Album4 Artiste4

Rang 4 Nombre Album 1 Debutant N 11 Album11 Artiste5

Rang 4 Nombre Album 1 Debutant N 11 Kalel' Bayo Accolade

La sélection d'un album se fait en cliquant sur l'image de celui-ci

par la procédure ClickImage l'information du N° Album se fait par le
**tag** de l'image

et la récupération des titres de l'album

Rappel **Sans Pagination**

En Cliquant sur un artiste par la procédure .ListBoxArtistesClick

GetAlbums →Obtient tous les albums triés par date de l'année de l'album

Creation_ImageEtiquettes → Affiche dans la scrollbox tous les albums de
l'artiste sélectionné

La selection de l'album se fait en cliquant sur l'image de celui-ci

le tag de l'image contient le N° Ordre de l'album de l'artiste

On à ce point vu toutes les options d'affichage Pousr les artistes et
pour les Albums

***Etude des sources de données***

****Après le fichier Text reliant les artistes et les albums on va
examiner le fichier au format JSON

Un fichier Json élaboré par un programme outil sert de base de données à
l'applicatif

Toutes les informations d'artistes d'albums et de titres sont extraites
de ce fichier

avec la structure suivante

Array →Objet → **{** Artiste

Album\[\] → TitreAlbum

Année

Song\[\] → Titre

NumPiste

**}**

Ajout des unités fpjson, jsonparser**,**

var

jData: TJSONData;

jItem : TJSONData;

jObject:TJSONObject;

*Récupération des Artistes*

JData:=GetJson(ReadFileToString(FileName));

for i := 0 to jData.Count - 1 do

begin

**jItem := jData.Items\[i\];**

**jObject:=TJSONObject(jItem);**

**Artiste:=jObject.Strings\['Artiste'\];**

ListBoxArtistes.Items.Add(Artiste);

end;

*Récupération des albums d'un Artiste*

procedure TForm1.ListBoxArtistesClick(Sender: TObject);

var

IndArtiste,j:integer;

TitreAlb,Annee:string;

jItem : TJSONData;

jObject,comp:TJSONObject;

JAlbum:TJSONArray;

begin

IndArtiste:=ListBoxArtistes.ItemIndex;

ArtisteSelect:=ListBoxArtistes.Items\[IndArtiste\];

**jItem := jData.Items\[IndArtiste\];**

**jObject:=TJSONObject(jItem);**

**JAlbum:=jObject.Arrays\['Album'\];**

Creation_ImageEtiquettes(Dimension1,JAlbum,ArtisteSelect);

****end; ****

Récupérationdes titres de l'album sélectionné

jItem := jData.Items\[Index\];

jObject:=TJSONObject(jItem);

JAlbum:=jObject.Arrays\['Album'\];

compObj:=JAlbum.Objects\[Tg\];

AlbumSelect:=compObj.Strings\['TitreAlb'\];

for i:=0 to JAlbum.Count-1 do

begin

compObj:=JAlbum.Objects\[i\];

TitreAlb:=compObj.Strings\['TitreAlb'\];

if TitreAlb= AlbumSelect then break;

end;

n:=i;

compObj:=JAlbum.Objects\[n\];

JTitre:=compObj.Arrays\['Song'\];

for i:=0 to JTitre.Count-1 do

begin

CompObj:=JTitre.Objects\[i\];

STitre:=compObj.Strings\['Titre'\];

end ;

***Etude Base de données Tablemem***

Ajout TmemDataset → Nom Table

On charge le fichier pour alimenter la table mémoire

**Table.Filename**:=SDrive+ChParam+'\TableMem\TableMem.json';

T**able.Active**:=True;;

FaitListBoxArtistes;

procedure TForm1.FaitListBoxArtistes;

var

MemoArtiste:string;

begin

ListBoxArtistes.Items.Clear;

**Table.First;**

MemoArtiste:='';

while not **Table.EOF** do

begin

if T**able.FieldByName**('Artiste').AsString\<\>MemoArtiste then

begin

ListBoxArtistes.Items.Add(Table.FieldByName('Artiste').AsString);

MemoArtiste:=Table.FieldByName('Artiste').AsString;

end;

Table.Next;

end;

ListBoxArtistes.ItemIndex:=0;

end; // FaitListBoxArtistes;

procedure TForm1.FaitAlbums;

var

i,CptAlbum:integer;

MemoArtiste:string;

procedure CompletetAlbum(NoAlb:integer;TemFinal:boolean);

var

j,i:integer;

S:string;

begin

j:=CptAlbum;

i:=NoAlb;

S:=Albums\[NoAlb\].NomAlbum;

if TemFinal then inc(NoAlb);

for i:= 1 to CptAlbum do

begin

Albums\[NoAlb-i\].NbAlbumArtiste:=CptAlbum ;

Albums\[NoAlb-i\].NoOrdreAlbCetArtiste:=j;

dec(j);

end;

end; // CompletetAlbum

begin

TotalAlbum:=0;

SetLength(Albums,0);

**Table.First;**

while not **Table.Eof** do

begin

SetLength(Albums,TotalAlbum+1);

Albums\[TotalAlbum\].NomArtiste:=**Table.FieldByName('Artiste').**AsString;

Albums\[TotalAlbum\].NomAlbum:=**Table.FieldByName('Album')**.AsString;

Albums\[TotalAlbum\].Annee:=**Table.FieldByName('Annee')**.AsInteger;

inc(TotalAlbum);

**Table.Next;**

end;

MemoArtiste:=''; //Albums\[0\].NomArtiste;

CptAlbum:=0;

for i:=0 to TotalAlbum-1 do

begin

if MemoArtiste\<\>Albums\[i\].NomArtiste then

begin

if CptAlbum \<\> 0 then CompletetAlbum(i,False);

CptAlbum:=1;

MemoArtiste:=Albums\[i\].NomArtiste;

end

else inc(CptAlbum);

end;

CompletetAlbum(i,True);

i:=0;

end; // FaitAlbums;

Le Filtrage permit par le composant enclencher par le code ci dessous et
met en œuvre l'évenement

Table.Close; permet le 'rescan' de la table

Table.Filtered:=True;

Table.Open;

.TableFilterRecord(DataSet: TDataSet; var Accept: Boolean);

chaque ligne de la table est examinée et si elle répond au critère de
filtrage on positionne la variable accept à true et la ligne sera
présente

***Etude de la Base de données SQLite3***

On Ajoute les composants

****SQL3C: TSQLite3Connection; SQL3C.KeepConnection=True
SQL3C.Transaction=SQLT

SQLQuery1: TSQLQuery; SQLQuery1.DataBase= SQL3C SQLQuery1.FieldDef

SQLQuery1.Transaction=SQLT

SQLQuery1.SQL=SELECT \* FROM Album

SQLT: TSQLTransaction SQLT.DataBase= SQL3C

***A l'initialisation***

****SQL3C.DatabaseName:=SDrive+ChParam+'\Music\Music.db';

SQLT.Active:=True;

SQLQuery1.Active:=True;

SQL3C.Connected:=True**;**

SQLQuery1.First;

SQLQuery1.EOF

SQLQuery1.FieldByName

SQLQuery1.Next;

procedure TForm1.Requete(SReq:string);

begin

SQLQuery1.Active:=False;

SQLQuery1.SQL\[0\]:=SReq;

SQLQuery1.ExecSQL;

SQLQuery1.Active:=true;

end; // Requete

Les requêtes

SELECT \* FROM Album WHERE album LIKE '+QuotedStr(EdFiltre.Text+'%');

SELECT \* FROM Album INNER JOIN Titre ON Album.idAlbum = Titre.NumAlbum
WHERE Titre.titre ='+QuotedStr(Sq); //+Title;

SELECT \* FROM Album WHERE annee BETWEEN '+QuotedStr(SD1)
AND'+QuotedStr(SD2);

***Etude par Balayage des répertoires***

Mise en place d'un objet de type TnomAlbum permettant le tri suivant un
critére défini par les besoins tri par année

TNomAlbum=class

private

fName: string;

fAnnee: integer;

public

constructor Create(NameAl:string; AnAlb:Integer);

property Name: string read fName write fName;

property Annee: integer read fAnnee write fAnnee;

end;

constructor TNomAlbum.Create(NameAl:string;AnAlb: Integer);

begin

fName:=NameAl;

fAnnee:=AnAlb;

end; // TNomAlbum.Create

function CompareAlbums(Item1, Item2: Pointer): Integer;

begin

// Compare les années des albums pour le tri

Result := TNomAlbum(Item1).Annee - TNomAlbum(Item2).Annee;

end; // CompareAlbums

**Artistes:TStringList**;

**NameAlb:TList;**

**NomAlbums,TLS:TStringList;**

Artistes := FindAllDirectories(CheminMorceaux,false);

Artistes.sort;

for i:=0 to Artistes.Count-1 do
ListBoxArtistes.Items.Add(ExtractFileName(Artistes\[i\]));

SDir:=Artistes\[i\];

NomAlbums:= FindAllDirectories(SDIr,false);

NameAlb:= TList.Create;

TLS:= TStringList.Create;

for j:=0 to NomAlbums.Count-1 do

begin

TLS.LoadFromFile(NomAlbums\[j\]+'\TagsParam.txt');

An:=StrToInt(TLS\[0\]);

NameAlb.Add(TNomAlbum.Create(NomAlbums\[j\],An));

end;

TLS.destroy;

NomAlbums.Destroy;

NameAlb.Sort(@CompareAlbums);

for i:=0 to NameAlb.Count-1 do

begin

SetLength(Albums,TotalAlbum+1);

Albums\[TotalAlbum\].NomArtiste:=ArtisteSel;

Albums\[TotalAlbum\].NomAlbum:=ExtractFileName(TNomAlbum(NameAlb\[i\]).Name);

Albums\[TotalAlbum\].Annee:=TNomAlbum(NameAlb\[i\]).Annee;

Albums\[TotalAlbum\].NbAlbumArtiste:=NameAlb.Count;

Albums\[TotalAlbum\].NoOrdreAlbCetArtiste:=i+1;

inc(TotalAlbum);

end;
