---
title: Interface et Système de Pagination
author: Tillot Alain
pubDatetime: 2025-12-20T15:33:05.569Z
slug: how-to-code-pascal-05-pagination
featured: false
draft: false
ogImage: ../../assets/images/forrest-gump-quote.png
tags:
  - Pascal
  - Lazarus
  - Pagination
  - Interface
description: Système complet de pagination pour afficher plusieurs artistes avec leurs albums.
---

# Interface et Système de Pagination

## Table des matières

1. [Motivation](#motivation)
2. [Calcul du nombre de pages](#calcul-du-nombre-de-pages)
3. [Structure des pages](#structure-des-pages)
4. [Dimensions et positionnement](#dimensions-et-positionnement)
5. [Implémentation](#implémentation)
6. [Navigation](#navigation)

---

## Motivation

Le système de pagination permet d'afficher **les albums de plusieurs artistes** sur un panel avec :

- Gestion automatique de la hauteur
- Navigation entre les pages
- Limite d'albums par ligne
- Affichage des pochettes avec étiquettes

**Exemple :** Afficher 7 albums par ligne sur un écran permettant 4 lignes, avec plusieurs artistes et leurs albums.

---

## Calcul du nombre de pages

### Paramètres

```pascal
const
  NbImagesParLigne = 7;      // Albums par ligne
  NbLignesParEcran = 4;      // Lignes par page
  MaxAlbumsPerPage = NbImagesParLigne * NbLignesParEcran;  // 28
```

### Calcul pour un artiste

```
Pour 17 albums et 7 albums par ligne :

Lignes complètes = 17 DIV 7 = 2 lignes
Albums restants = 17 MOD 7 = 3 albums

→ Nombre de lignes totales = 3 lignes
```

### Tableau de démonstration

#### Modulo 7 (7 images par ligne)

```
N     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
DIV   0 0 0 0 0 0 0 1 1 1  1  1  1  1  2  2  2  2  2
MOD   0 1 2 3 4 5 6 0 1 2  3  4  5  6  0  1  2  3  4
```

#### Modulo 8 (8 images par ligne)

```
N     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
DIV   0 0 0 0 0 0 0 0 1 1  1  1  1  1  1  1  2  2  2
MOD   0 1 2 3 4 5 6 7 0 1  2  3  4  5  6  7  0  1  2
```

#### Modulo 5 (5 images par ligne)

```
N     0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
DIV   0 0 0 0 0 1 1 1 1 1  2  2  2  2  2  3  3  3  3
MOD   0 1 2 3 4 0 1 2 3 4  0  1  2  3  4  0  1  2  3
```

### Calcul du nombre total de pages

```pascal
function CalculateNumberOfPages(TotalLines: integer): integer;
begin
  if TotalLines <= NbLignesParEcran then
    Result := 1
  else
    Result := TotalLines - NbLignesParEcran + 1;
end;

// Exemple : 12 lignes totales, 4 lignes par écran
// Pages = 12 - 4 + 1 = 9 pages
```

---

## Structure des pages

### Type de données pour une page

```pascal
type
  PageLineInfo = record
    NoDisqueDébutantLigne: array[1..MaxLinesPerPage] of integer;
    NbreAlbum: array[1..MaxLinesPerPage] of integer;
  end;

var
  Pages: array of PageLineInfo;
```

### Exemple de tableau Pages

**Page 0 :**
```
Rang 1 | NbAlbums: 1 | Début: 0  | Album1 Artiste1
Rang 2 | NbAlbums: 2 | Début: 1  | Album2 Artiste2
Rang 3 | NbAlbums: 1 | Début: 3  | Album3 Artiste3
Rang 4 | NbAlbums: 1 | Début: 4  | Album4 Artiste4
```

**Page 1 :**
```
Rang 1 | NbAlbums: 2 | Début: 1  | Album2 Artiste2
Rang 2 | NbAlbums: 1 | Début: 3  | Album3 Artiste3
Rang 3 | NbAlbums: 7 | Début: 4  | Album4 Artiste4
Rang 4 | NbAlbums: 1 | Début: 11 | Album11 Artiste5
```

---

## Dimensions et positionnement

### Structure de dimensions

```pascal
type
  PaginationDimension = record
    MargeTop: integer;          // Marge haut (5px)
    DimImage: integer;          // Taille image (130px)
    MargeTitre: integer;        // Marge titre (8px)
    HtTitreAlb: integer;        // Hauteur titre (16px)
    HtEtiqYear: integer;        // Hauteur année (10px)
    MargeLeft: integer;         // Marge gauche
    MargeInter: integer;        // Marge entre images
    HtEtiqArtiste: integer;     // Hauteur étiquette artiste (11px)
    MargeImage: integer;        // Marge image (22px)
    MargeInterOp: integer;      // Marge inter opération
  end;

const
  Dimension1: PaginationDimension = (
    MargeTop: 5;
    DimImage: 130;
    MargeTitre: 8;
    HtTitreAlb: 16;
    HtEtiqYear: 10;
    MargeLeft: 10;
    MargeInter: 20;
    HtEtiqArtiste: 11;
    MargeImage: 22;
    MargeInterOp: 0
  );
```

### Calcul des positions Y

```pascal
// Pas fixe pour chaque ligne d'albums
PasFixe := Dim.MargeTop + Dim.DimImage + Dim.MargeTitre + 
           Dim.HtTitreAlb + Dim.HtEtiqYear;
// = 5 + 130 + 8 + 16 + 10 = 169px

// Pas variable ajoute à chaque changement d'artiste
PasVariable := PasVariable + Dim.HtEtiqArtiste + Dim.MargeImage;
// = 11 + 22 = 33px

// Position Y d'une image
PosImageY := Dim.MargeTop + PasVariable + (PasFixe * (NumLigne - 1));

// Positionnement des étiquettes
EtiqAlbum.Top := PosImageY + Dim.DimImage + Dim.MargeTitre;
EtiqYear.Top := PosImageY + Dim.DimImage + Dim.MargeTitre + Dim.HtTitreAlb;
```

---

## Implémentation

### Procédure principale de préparation des pages

```pascal
procedure TForm1.PreparePages(Dim: PaginationDimension);
var
  i, NumPage, NumLigneActuelle: integer;
  NumAlbumActuel: integer;
begin
  // Nettoie les pages précédentes
  SetLength(Pages, 0);
  
  NumPage := 0;
  NumLigneActuelle := 1;
  NumAlbumActuel := 0;
  
  SetLength(Pages, 1);  // Première page
  
  // Remplir les pages
  while NumAlbumActuel < TotalAlbum do
  begin
    // Ajoute une ligne à la page actuelle
    PrepareLineForPage(NumAlbumActuel, NumLigneActuelle, 
                      NumPage, Dim);
    
    // Passe à la ligne suivante
    Inc(NumLigneActuelle);
    
    // Si la page est pleine, crée une nouvelle page
    if NumLigneActuelle > NbLignesParEcran then
    begin
      // La dernière ligne de la page devient la première de la suivante
      NumLigneActuelle := 2;
      Inc(NumPage);
      SetLength(Pages, NumPage + 1);
    end;
  end;
end;

procedure TForm1.PrepareLineForPage(var NumAlbum: integer; 
                                    NumLigne: integer;
                                    NumPage: integer;
                                    Dim: PaginationDimension);
var
  NbAlbumsThisLine: integer;
begin
  // Compte les albums à afficher sur cette ligne
  NbAlbumsThisLine := 0;
  
  // Itère jusqu'à remplir une ligne
  while (NumAlbum < TotalAlbum) and 
        (NbAlbumsThisLine < NbImagesParLigne) do
  begin
    Inc(NbAlbumsThisLine);
    Inc(NumAlbum);
  end;
  
  // Enregistre dans la structure de la page
  with Pages[NumPage] do
  begin
    NoDisqueDébutantLigne[NumLigne] := NumAlbum - NbAlbumsThisLine;
    NbreAlbum[NumLigne] := NbAlbumsThisLine;
  end;
end;
```

### Affichage d'une page

```pascal
procedure TForm1.DisplayPage(PageNumber: integer; 
                             Dim: PaginationDimension);
var
  NumLigne: integer;
  NumAlbumDisplay: integer;
  NumAlbumFirst: integer;
  NbAlbumsToDisplay: integer;
begin
  // Nettoie les composants précédents
  ClearPageComponents;
  
  // Affiche chaque ligne de la page
  for NumLigne := 1 to NbLignesParEcran do
  begin
    with Pages[PageNumber] do
    begin
      NumAlbumFirst := NoDisqueDébutantLigne[NumLigne];
      NbAlbumsToDisplay := NbreAlbum[NumLigne];
      
      if NbAlbumsToDisplay > 0 then
      begin
        // Affiche l'étiquette artiste si premier album
        if NumLigne = 1 then
          DisplayArtistLabel(Albums[NumAlbumFirst].NomArtiste, 
                           NumLigne, Dim);
        
        // Affiche les albums de cette ligne
        for NumAlbumDisplay := 0 to NbAlbumsToDisplay - 1 do
        begin
          DisplayAlbumImage(
            NumAlbumFirst + NumAlbumDisplay,
            NumLigne,
            NumAlbumDisplay,
            Dim
          );
        end;
      end;
    end;
  end;
  
  CurrentPageNumber := PageNumber;
end;

procedure TForm1.DisplayAlbumImage(AlbumIndex: integer;
                                   LineNumber: integer;
                                   ColumnNumber: integer;
                                   Dim: PaginationDimension);
var
  PosImageY, PosImageX: integer;
  UneImage: TImage;
  UneEtiquette: TLabel;
  SImage: string;
begin
  // Calcul des positions
  PosImageX := Dim.MargeLeft + ColumnNumber * 
               (Dim.DimImage + Dim.MargeInter);
  PosImageY := Dim.MargeTop + (LineNumber - 1) * Dim.DimImage;
  
  // Création de l'image
  UneImage := TImage.Create(Self);
  with UneImage do
  begin
    Parent := PanelAlbums;
    Height := Dim.DimImage;
    Width := Dim.DimImage;
    Top := PosImageY;
    Left := PosImageX;
    Stretch := true;
    
    SImage := CheminMorceaux + Albums[AlbumIndex].NomArtiste + '\' +
              Albums[AlbumIndex].NomAlbum + '\cover.jpg';
    
    if FileExists(SImage) then
      Picture.LoadFromFile(SImage);
    
    OnClick := @Form1.ClickImage;
    Name := 'Image' + IntToStr(AlbumIndex);
    Visible := true;
    Tag := AlbumIndex;
  end;
  
  // Création de l'étiquette album
  UneEtiquette := TLabel.Create(Self);
  with UneEtiquette do
  begin
    Parent := PanelAlbums;
    Top := PosImageY + Dim.DimImage + Dim.MargeTitre;
    Left := PosImageX;
    Caption := Albums[AlbumIndex].NomAlbum;
    Font.Size := 10;
    Font.Color := clBlack;
    Name := 'EtiqAlb' + IntToStr(AlbumIndex);
  end;
  
  // Création de l'étiquette année
  UneEtiquette := TLabel.Create(Self);
  with UneEtiquette do
  begin
    Parent := PanelAlbums;
    Top := PosImageY + Dim.DimImage + Dim.MargeTitre + 
           Dim.HtTitreAlb;
    Left := PosImageX;
    Caption := IntToStr(Albums[AlbumIndex].Annee);
    Font.Size := 8;
    Font.Color := clGray;
    Name := 'EtiqYear' + IntToStr(AlbumIndex);
  end;
end;

procedure TForm1.DisplayArtistLabel(ArtistName: string;
                                    LineNumber: integer;
                                    Dim: PaginationDimension);
var
  UneEtiquette: TLabel;
begin
  UneEtiquette := TLabel.Create(Self);
  with UneEtiquette do
  begin
    Parent := PanelAlbums;
    Top := Dim.MargeTop + (LineNumber - 2) * 
           (Dim.DimImage + Dim.MargeTitre + Dim.HtTitreAlb);
    Left := Dim.MargeLeft;
    Caption := ArtistName;
    Font.Size := 12;
    Font.Style := [fsBold];
    Font.Color := clBlue;
    Name := 'EtiqArt' + IntToStr(LineNumber);
  end;
end;

procedure TForm1.ClearPageComponents;
var
  i: integer;
begin
  for i := ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TImage then
      Components[i].Free
    else if Components[i] is TLabel then
      if (Pos('Etiq', Components[i].Name) = 1) then
        Components[i].Free;
  end;
end;
```

---

## Navigation

### Boutons de navigation

```pascal
procedure TForm1.BtnPreviousPageClick(Sender: TObject);
begin
  if CurrentPageNumber > 0 then
  begin
    Dec(CurrentPageNumber);
    DisplayPage(CurrentPageNumber, Dimension1);
    UpdatePageIndicator;
  end;
end;

procedure TForm1.BtnNextPageClick(Sender: TObject);
var
  MaxPage: integer;
begin
  MaxPage := High(Pages);
  
  if CurrentPageNumber < MaxPage then
  begin
    Inc(CurrentPageNumber);
    DisplayPage(CurrentPageNumber, Dimension1);
    UpdatePageIndicator;
  end;
end;

procedure TForm1.UpdatePageIndicator;
begin
  LblPageIndicator.Caption := 
    'Page ' + IntToStr(CurrentPageNumber + 1) + 
    ' / ' + IntToStr(Length(Pages));
end;
```

### Affichage initial

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Charge les données
  LoadArtistsFromDirs;
  LoadAlbumsFromDirs;
  
  // Prépare les pages
  PreparePages(Dimension1);
  
  // Affiche la première page
  CurrentPageNumber := 0;
  DisplayPage(0, Dimension1);
  UpdatePageIndicator;
end;
```

---

## Résumé des fichiers

Voir aussi les guides précédents :
- [01-introduction.md](01-introduction.md) - Architecture générale
- [02-gestion-artistes-albums.md](02-gestion-artistes-albums.md) - ScrollBox et TreeView
- [03-lecteur-playlists.md](03-lecteur-playlists.md) - Lecteur audio
- [04-sources-donnees.md](04-sources-donnees.md) - Sources de données

Fin de la série !
