---
title: Gestion des Artistes et Albums en Pascal
author: Tillot Alain
pubDatetime: 2025-12-20T15:33:05.569Z
slug: how-to-code-pascal-02-artistes-albums
featured: false
draft: false
ogImage: ../../assets/images/forrest-gump-quote.png
tags:
  - Pascal
  - Lazarus
  - Composants UI
description: Implémentation de l'interface artistes et albums avec ListBox, TreeView et ScrollBox.
---

# Gestion des Artistes et Albums

## Table des matières

1. [Structures de données](#structures-de-données)
2. [Affichage des artistes](#affichage-des-artistes)
3. [Gestion des albums - ScrollBox](#gestion-des-albums---scrollbox)
4. [Gestion des albums - TreeView](#gestion-des-albums---treeview)
5. [Système de pagination](#système-de-pagination)

---

## Structures de données

### Type d'album

```pascal
type
  AlbTyp = record
    NomArtiste: string;
    NomAlbum: string;
    Annee: integer;
    NbAlbumArtiste: integer;      // Nombre total d'albums de cet artiste
    NoOrdreAlbCetArtiste: integer; // Numéro d'ordre pour cet artiste
  end;
```

### Variables globales

```pascal
var
  Artistes: TStringList;          // Liste des artistes
  Albums: array of AlbTyp;        // Tableau dynamique des albums
  TotalAlbum: integer = 0;        // Nombre total d'albums
  ArtisteSelect: string;          // Artiste actuellement sélectionné
  AlbumSelect: string;            // Album actuellement sélectionné
```

### Configuration des répertoires

```pascal
procedure GetDrive;
var
  SDrive: string;
  ChParam: string;
  CheminMorceaux: string;
  CheminPlayListe: string;
begin
  // Recherche du lecteur contenant UnHommeAverti.txt
  SDrive := 'd:\\';  // Exemple
  ChParam := 'MusiqueParam';
  CheminMorceaux := SDrive + 'Music\\';
  CheminPlayListe := SDrive + ChParam + '\\PlayListe\\';
end;
```

---

## Affichage des artistes

### Chargement simple avec ListBox

```pascal
procedure LoadArtistes;
var
  i: integer;
  Artistes: TStringList;
begin
  ListBoxArtistes.Items.Clear;
  
  // Récupère tous les répertoires du dossier Music
  Artistes := FindAllDirectories(CheminMorceaux, false);
  Artistes.Sort;
  
  // Ajoute chaque artiste à la ListBox
  for i := 0 to Artistes.Count - 1 do
    ListBoxArtistes.Items.Add(ExtractFileName(Artistes[i]));
end;
```

### Gestion du clic sur un artiste

```pascal
procedure TForm1.ListBoxArtistesClick(Sender: TObject);
var
  IndArtiste: integer;
begin
  IndArtiste := ListBoxArtistes.ItemIndex;
  ArtisteSelect := ListBoxArtistes.Items[IndArtiste];
  
  // Charge les albums de cet artiste
  LoadAlbumsForArtist(ArtisteSelect);
end;
```

---

## Gestion des albums - ScrollBox

### Lecture du fichier texte Artiste.txt

Chaque artiste a un fichier `NomArtiste.txt` avec la structure :

```
Album1
Annee1
Album2
Annee2
...
AlbumN
AnneeN
```

### Procédure de chargement

```pascal
procedure TForm1.LoadAlbumsFromFile(ArtistName: string);
var
  SDir: string;
  i: integer;
  TL: TStringList;
begin
  TL := TStringList.Create;
  try
    // Chemin du fichier artiste
    SDir := SDrive + ChParam + '\Albums\' + ArtistName + '.txt';
    
    if FileExists(SDir) then
    begin
      TL.LoadFromFile(SDir);
      
      // Traite les lignes par paires (Album, Année)
      for i := 0 to TL.Count - 1 do
      begin
        if i mod 2 = 0 then  // Ligne album
        begin
          SetLength(Albums, TotalAlbum + 1);
          Albums[TotalAlbum].NomAlbum := TL[i];
          Albums[TotalAlbum].NomArtiste := ArtistName;
        end
        else  // Ligne année
        begin
          Albums[TotalAlbum].Annee := StrToInt(TL[i]);
          Inc(TotalAlbum);
        end;
      end;
    end;
  finally
    TL.Free;
  end;
end;
```

### Dimensions pour l'affichage

```pascal
type
  Dimension = record
    DimImage: integer;      // Taille de l'image (180px)
    MargeLeft: integer;     // Marge gauche (210px)
    MargeTop: integer;      // Marge haut (15px)
    MargeInter: integer;    // Marge entre images (20px)
    MargeTitre: integer;    // Marge titre (8px)
    MargeImage: integer;    // Marge image (22px)
    HtTitreAlb: integer;    // Hauteur titre album (16px)
    HtEtiqYear: integer;    // Hauteur étiquette année (10px)
  end;

const
  Dimension1: Dimension = (
    DimImage: 180;
    MargeLeft: 210;
    MargeTop: 15;
    MargeInter: 20;
    MargeTitre: 8;
    MargeImage: 22;
    HtTitreAlb: 16;
    HtEtiqYear: 10
  );
```

### Création des images et étiquettes

```pascal
procedure TForm1.CreateAlbumImagesAndLabels(Dim: Dimension);
var
  Col, PasX, i: integer;
  SImage: string;
  Comp: TComponent;
  UneImage: TImage;
  UneEtiquette: TLabel;
begin
  ListBoxTitres.Items.Clear;
  
  // Supprime les images et étiquettes précédentes
  for i := Form1.ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TImage then
    begin
      UneImage := Components[i] as TImage;
      SImage := Copy(UneImage.Name, 1, 5);
      if SImage = 'Image' then
        Components[i].Free;
    end;
    
    Comp := FindComponent('Etiq' + IntToStr(i));
    if Comp is TLabel then
      Comp.Free;
    
    Comp := FindComponent('EtiqY' + IntToStr(i));
    if Comp is TLabel then
      Comp.Free;
  end;
  
  // Calcul du pas horizontal
  PasX := Dim.DimImage + Dim.MargeInter;
  
  // Crée les images et étiquettes pour chaque album
  for Col := 0 to TotalAlbum - 1 do
  begin
    // Création de l'image
    UneImage := TImage.Create(Self);
    with UneImage do
    begin
      Parent := ScrollBoxAlbums;
      Height := Dim.DimImage;
      Width := Dim.DimImage;
      Top := Dim.MargeTop;
      Left := Dim.MargeLeft + (Col - 1) * PasX;
      Stretch := true;
      
      // Charge la pochette
      SImage := CheminMorceaux + ArtisteSelect + '\' + 
                Albums[Col].NomAlbum + '\cover.jpg';
      Picture.LoadFromFile(SImage);
      
      OnClick := @Form1.ClickImage;
      Name := 'Image' + IntToStr(Col);
      Visible := true;
      Hint := 'Cliquer pour obtenir les titres';
      ShowHint := true;
      Tag := Col;  // Stocke l'index
    end;
    
    // Création de l'étiquette album
    UneEtiquette := TLabel.Create(Self);
    with UneEtiquette do
    begin
      Parent := ScrollBoxAlbums;
      Top := Dim.MargeTop + Dim.DimImage;
      Left := 10 + Dim.MargeLeft + (Col - 1) * PasX;
      Font.Size := 12;
      Font.Color := clBlack;
      Font.Style := [fsBold];
      Name := 'Etiq' + IntToStr(Col);
      Caption := Albums[Col].NomAlbum;
    end;
    
    // Création de l'étiquette année
    UneEtiquette := TLabel.Create(Self);
    with UneEtiquette do
    begin
      Parent := ScrollBoxAlbums;
      Top := 10 + Dim.MargeTop + Dim.DimImage + Dim.HtTitreAlb;
      Left := 10 + Dim.MargeLeft + (Col - 1) * PasX;
      Font.Size := 10;
      Font.Color := clBlack;
      Font.Style := [fsBold];
      Name := 'EtiqY' + IntToStr(Col);
      Caption := IntToStr(Albums[Col].Annee);
    end;
  end;
end;
```

### Gestion du clic sur une image

```pascal
procedure TForm1.ClickImage(Sender: TObject);
var
  Tg, i, NbTitres: integer;
  Image: TImage;
  SImage, NameFich, LigUTF8: string;
  TL: TStringList;
begin
  Image := Sender as TImage;
  Tg := Image.Tag;
  
  // Affiche la pochette sélectionnée
  SImage := CheminMorceaux + ArtisteSelect + '\' + 
            Albums[Tg].NomAlbum + '\cover.jpg';
  CoverSel.Picture.LoadFromFile(SImage);
  CoverSel.Visible := true;
  
  // Affiche les infos album
  LblSelAlbum.Visible := true;
  LblSelArtiste.Visible := true;
  LblSelAlbum.Caption := ' ' + Albums[Tg].NomAlbum + ' ';
  LblSelArtiste.Caption := ' ' + ArtisteSelect + ' ';
  
  AlbumSelect := Albums[Tg].NomAlbum;
  
  // Charge les titres
  ListBoxTitres.Items.Clear;
  NameFich := CheminMorceaux + ArtisteSelect + '\' + 
              Albums[Tg].NomAlbum + '\TagsParam.txt';
  
  TL := TStringList.Create;
  try
    if FileExists(NameFich) then
    begin
      TL.LoadFromFile(NameFich);
      
      // Première ligne : année
      TL.Delete(0);
      
      // Deuxième ligne : nombre de titres
      NbTitres := StrToInt(TL[0]);
      TL.Delete(0);
      
      // Charge les titres
      for i := 0 to NbTitres - 1 do
      begin
        LigUTF8 := ISO_8859_1ToUTF8(TL[i]);
        ListBoxTitres.Items.Add(LigUTF8);
      end;
    end;
  finally
    TL.Free;
  end;
end;
```

---

## Gestion des albums - TreeView

### Avantages

- Hiérarchie visuelle Artiste-Album
- Plus compacte qu'une ScrollBox
- Navigation intuitive

### Construction du TreeView

```pascal
procedure TForm1.BuildTreeView;
var
  i: integer;
  Node: TTreeNode;
  MemoArtiste: string;
begin
  TotalAlbum := 0;
  
  // Charge tous les albums triés par artiste
  for i := 0 to Artistes.Count - 1 do
    LoadAlbumsFromFile(ExtractFileName(Artistes[i]));
  
  // Construit l'arbre
  TreeView1.Items.Clear;
  MemoArtiste := '';
  Node := nil;
  
  for i := 0 to TotalAlbum - 1 do
  begin
    if Albums[i].NomArtiste <> MemoArtiste then
    begin
      // Nouveau nœud artiste
      Node := TreeView1.Items.Add(nil, Albums[i].NomArtiste);
      MemoArtiste := Albums[i].NomArtiste;
    end;
    
    // Ajoute l'album comme enfant
    TreeView1.Items.AddChild(Node, Albums[i].NomAlbum);
  end;
end;
```

---

## Système de pagination

### Motivation

Pour afficher les pochettes d'**plusieurs artistes** sur un panel avec gestion des pages.

### Calcul du nombre de lignes

Avec une matrice de `NbImagesParLigne × NbLignesParEcran` :

Pour un artiste :
- Lignes complètes = `NbAlbums DIV NbImagesParLigne`
- Albums sur la ligne incomplète = `NbAlbums MOD NbImagesParLigne`

### Exemple avec 17 albums et 7 images par ligne

```
Lignes complètes: 17 DIV 7 = 2
Albums restants: 17 MOD 7 = 3

→ Total: 3 lignes nécessaires
```

### Tableau de structure pour les pages

```pascal
type
  PageInfo = record
    NoDisqueDébutantLigne: array[1..MaxLinesPerPage] of integer;
    NbreAlbum: array[1..MaxLinesPerPage] of integer;
  end;
```

### Préparation des pages

```pascal
procedure PrepPage(NoAlbDebutPage: integer; NumPage: integer; 
                   NbImgParLigne: integer);
var
  i, Col: integer;
  PosImage, PasFixe, PasVariable: integer;
begin
  PasFixe := MargeTop + DimImage + MargeTitre + HtTitreAlb + HtEtiqYear;
  // Valeurs: 5 + 130 + 8 + 16 + 10 = 169
  
  for i := 0 to High(Albums) do
  begin
    if Albums[i].NomArtiste <> MemoArtiste then
    begin
      PasVariable := PasVariable + HtEtiqArtiste + MargeImage;
      MemoArtiste := Albums[i].NomArtiste;
    end;
    
    Col := i mod NbImgParLigne;
    PosImage := MargeTop + PasVariable + 
                (PasFixe * (i div NbImgParLigne));
    
    // Positionnement des images
    ImageAlb[i].Top := PosImage;
    EtiqAlb[i].Top := PosImage + DimImage + MargeTitre;
    EtiqYear[i].Top := PosImage + DimImage + MargeTitre + HtTitreAlb;
  end;
end;
```

---

## Prochaines étapes

Voir [03-lecteur-audio-playlists.md](03-lecteur-audio-playlists.md) pour l'implémentation du lecteur audio.
