---
title: Sources de Données - JSON, Base de Données, Table Mémoire
author: Tillot Alain
pubDatetime: 2025-12-20T15:33:05.569Z
slug: how-to-code-pascal-04-sources-donnees
featured: false
draft: false
ogImage: ../../assets/images/forrest-gump-quote.png
tags:
  - Pascal
  - Lazarus
  - JSON
  - Base de données
  - SQLite
description: Comparaison et implémentation de 4 sources de données pour l'application musicale.
---

# Sources de Données

## Table des matières

1. [Vue d'ensemble](#vue-densemble)
2. [Format JSON](#format-json)
3. [Table en mémoire](#table-en-mémoire)
4. [Base de données SQLite](#base-de-données-sqlite)
5. [Balayage des répertoires](#balayage-des-répertoires)

---

## Vue d'ensemble

| Source | Avantages | Inconvénients |
|--------|-----------|---------------|
| **Fichier texte** | Simple, léger | Mise à jour manuelle |
| **JSON** | Flexible, structuré | Parsing requis |
| **Table en mémoire** | Rapide, sans dépendance | Mémoire limitée |
| **SQLite** | Scalable, requêtes complexes | Fichier à distribuer |
| **Balayage répertoires** | Automatique, dynamique | Performance dépend du système |

---

## Format JSON

### Structure du fichier JSON

```json
[
  {
    "Artiste": "Artiste1",
    "Album": [
      {
        "TitreAlb": "Album1",
        "Annee": 1998,
        "Song": [
          {
            "Titre": "Chanson 1",
            "NumPiste": 1
          },
          {
            "Titre": "Chanson 2",
            "NumPiste": 2
          }
        ]
      },
      {
        "TitreAlb": "Album2",
        "Annee": 2001,
        "Song": [
          {
            "Titre": "Chanson 1",
            "NumPiste": 1
          }
        ]
      }
    ]
  }
]
```

### Unités requises

```pascal
uses
  fpjson,
  jsonparser;
```

### Lecture du fichier JSON

```pascal
var
  jData: TJSONData;
  jItem: TJSONData;
  jObject: TJSONObject;

procedure TForm1.LoadFromJSON(FileName: string);
var
  i: integer;
  Artiste: string;
begin
  // Charge le fichier JSON
  jData := GetJSON(ReadFileToString(FileName));
  
  ListBoxArtistes.Items.Clear;
  
  // Parcourt les artistes
  for i := 0 to jData.Count - 1 do
  begin
    jItem := jData.Items[i];
    jObject := TJSONObject(jItem);
    
    Artiste := jObject.Strings['Artiste'];
    ListBoxArtistes.Items.Add(Artiste);
  end;
end;
```

### Récupération des albums d'un artiste

```pascal
procedure TForm1.ListBoxArtistesClick(Sender: TObject);
var
  IndArtiste: integer;
  ArtisteSelect: string;
  jItem: TJSONData;
  jObject: TJSONObject;
  JAlbum: TJSONArray;
begin
  IndArtiste := ListBoxArtistes.ItemIndex;
  ArtisteSelect := ListBoxArtistes.Items[IndArtiste];
  
  // Récupère l'objet artiste
  jItem := jData.Items[IndArtiste];
  jObject := TJSONObject(jItem);
  
  // Récupère le tableau des albums
  JAlbum := jObject.Arrays['Album'];
  
  // Crée les images des albums
  CreateAlbumImagesAndLabels(Dimension1, JAlbum, ArtisteSelect);
end;
```

### Récupération des titres d'un album

```pascal
procedure TForm1.ClickImage(Sender: TObject);
var
  Index, Tg, i, n: integer;
  jItem: TJSONData;
  jObject, compObj: TJSONObject;
  JAlbum: TJSONArray;
  JTitre: TJSONArray;
  AlbumSelect, STitre: string;
begin
  Index := ListBoxArtistes.ItemIndex;
  Tg := (Sender as TImage).Tag;
  
  // Récupère les albums de l'artiste sélectionné
  jItem := jData.Items[Index];
  jObject := TJSONObject(jItem);
  JAlbum := jObject.Arrays['Album'];
  
  // Récupère l'album sélectionné
  compObj := JAlbum.Objects[Tg];
  AlbumSelect := compObj.Strings['TitreAlb'];
  
  // Cherche l'index de l'album
  for i := 0 to JAlbum.Count - 1 do
  begin
    compObj := JAlbum.Objects[i];
    if compObj.Strings['TitreAlb'] = AlbumSelect then
      break;
  end;
  n := i;
  
  // Récupère les titres
  compObj := JAlbum.Objects[n];
  JTitre := compObj.Arrays['Song'];
  
  ListBoxTitres.Items.Clear;
  
  for i := 0 to JTitre.Count - 1 do
  begin
    compObj := JTitre.Objects[i];
    STitre := compObj.Strings['Titre'];
    ListBoxTitres.Items.Add(STitre);
  end;
end;
```

---

## Table en mémoire

### Configuration du composant

```pascal
var
  Table: TMemDataset;

procedure TForm1.InitializeMemoryTable;
begin
  Table.Filename := SDrive + ChParam + '\TableMem\TableMem.json';
  Table.Active := True;
  
  LoadArtists;
end;
```

### Chargement des artistes

```pascal
procedure TForm1.LoadArtists;
var
  MemoArtiste: string;
begin
  ListBoxArtistes.Items.Clear;
  
  Table.First;
  MemoArtiste := '';
  
  // Récupère les artistes uniques
  while not Table.EOF do
  begin
    if Table.FieldByName('Artiste').AsString <> MemoArtiste then
    begin
      ListBoxArtistes.Items.Add(Table.FieldByName('Artiste').AsString);
      MemoArtiste := Table.FieldByName('Artiste').AsString;
    end;
    
    Table.Next;
  end;
  
  ListBoxArtistes.ItemIndex := 0;
end;
```

### Chargement des albums

```pascal
procedure TForm1.LoadAlbums;
var
  i, CptAlbum: integer;
  MemoArtiste: string;
begin
  TotalAlbum := 0;
  SetLength(Albums, 0);
  
  Table.First;
  
  // Charge tous les albums
  while not Table.EOF do
  begin
    SetLength(Albums, TotalAlbum + 1);
    
    Albums[TotalAlbum].NomArtiste := 
      Table.FieldByName('Artiste').AsString;
    Albums[TotalAlbum].NomAlbum := 
      Table.FieldByName('Album').AsString;
    Albums[TotalAlbum].Annee := 
      Table.FieldByName('Annee').AsInteger;
    
    Inc(TotalAlbum);
    Table.Next;
  end;
  
  // Complète les infos
  MemoArtiste := '';
  CptAlbum := 0;
  
  for i := 0 to TotalAlbum - 1 do
  begin
    if MemoArtiste <> Albums[i].NomArtiste then
    begin
      if CptAlbum <> 0 then
        CompleteAlbumInfo(i, False);
      
      CptAlbum := 1;
      MemoArtiste := Albums[i].NomArtiste;
    end
    else
      Inc(CptAlbum);
  end;
  
  CompleteAlbumInfo(i, True);
end;

procedure TForm1.CompleteAlbumInfo(NoAlb: integer; IsFinal: boolean);
var
  j, i, CptAlbum: integer;
begin
  j := CptAlbum;
  i := NoAlb;
  
  if IsFinal then
    Inc(NoAlb);
  
  for i := 1 to CptAlbum do
  begin
    Albums[NoAlbum - i].NbAlbumArtiste := CptAlbum;
    Albums[NoAlbum - i].NoOrdreAlbCetArtiste := j;
    Dec(j);
  end;
end;
```

### Filtrage

```pascal
procedure TForm1.FilterTable(FilterCriteria: string);
begin
  // Ferme la table pour activer le recalcul
  Table.Close;
  
  // Applique le filtre
  Table.Filter := FilterCriteria;
  Table.Filtered := True;
  
  // Réouvre la table
  Table.Open;
end;

// Événement de filtrage ligne par ligne
procedure TForm1.TableFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  // Vérifie si la ligne répond aux critères
  Accept := (DataSet.FieldByName('Artiste').AsString = ArtisteSelect);
end;
```

---

## Base de données SQLite

### Composants requis

```pascal
uses
  SQLdb,
  sqlite3connection;

var
  SQL3C: TSQLite3Connection;
  SQLQuery1: TSQLQuery;
  SQLT: TSQLTransaction;
```

### Configuration

```pascal
procedure TForm1.InitializeSQLite;
begin
  // Configure la connexion
  SQL3C.DatabaseName := SDrive + ChParam + '\Music\Music.db';
  SQL3C.KeepConnection := True;
  SQL3C.Transaction := SQLT;
  
  // Configure la transaction
  SQLT.Database := SQL3C;
  SQLT.Active := True;
  
  // Configure la requête
  SQLQuery1.Database := SQL3C;
  SQLQuery1.Transaction := SQLT;
  SQLQuery1.SQL.Text := 'SELECT * FROM Album';
  
  // Établit la connexion
  SQL3C.Connected := True;
  SQLQuery1.Active := True;
  
  SQLQuery1.First;
end;
```

### Requêtes courantes

#### Sélection simple

```pascal
procedure TForm1.SelectAllAlbums;
begin
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 'SELECT * FROM Album ORDER BY Album';
  SQLQuery1.Active := True;
  
  ListBoxAlbums.Items.Clear;
  
  SQLQuery1.First;
  while not SQLQuery1.EOF do
  begin
    ListBoxAlbums.Items.Add(
      SQLQuery1.FieldByName('Album').AsString);
    SQLQuery1.Next;
  end;
end;
```

#### Recherche avec LIKE

```pascal
procedure TForm1.SearchAlbum(SearchTerm: string);
begin
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 
    'SELECT * FROM Album WHERE Album LIKE ' + 
    QuotedStr(SearchTerm + '%');
  SQLQuery1.Active := True;
end;
```

#### Jointure

```pascal
procedure TForm1.GetAlbumWithTitles(AlbumName: string);
begin
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 
    'SELECT * FROM Album ' +
    'INNER JOIN Titre ON Album.idAlbum = Titre.NumAlbum ' +
    'WHERE Album.Album = ' + QuotedStr(AlbumName);
  SQLQuery1.Active := True;
end;
```

#### Requête entre deux dates

```pascal
procedure TForm1.GetAlbumsBetweenYears(Year1, Year2: integer);
begin
  SQLQuery1.Active := False;
  SQLQuery1.SQL.Text := 
    'SELECT * FROM Album WHERE Annee BETWEEN ' + 
    QuotedStr(IntToStr(Year1)) + ' AND ' + 
    QuotedStr(IntToStr(Year2)) +
    ' ORDER BY Annee';
  SQLQuery1.Active := True;
end;
```

### Exécution de requêtes

```pascal
procedure TForm1.ExecuteQuery(SQLStatement: string);
begin
  SQLQuery1.Active := False;
  SQLQuery1.SQL[0] := SQLStatement;
  SQLQuery1.ExecSQL;
  SQLQuery1.Active := True;
end;
```

---

## Balayage des répertoires

### Classe pour l'ordre de tri

```pascal
type
  TNomAlbum = class
  private
    fName: string;
    fAnnee: integer;
  public
    constructor Create(NameAl: string; AnAlb: Integer);
    property Name: string read fName write fName;
    property Annee: integer read fAnnea write fAnnea;
  end;

constructor TNomAlbum.Create(NameAl: string; AnAlb: Integer);
begin
  fName := NameAl;
  fAnnea := AnAlb;
end;
```

### Fonction de comparaison pour le tri

```pascal
function CompareAlbums(Item1, Item2: Pointer): Integer;
begin
  // Trie par année (ascendant)
  Result := TNomAlbum(Item1).Annea - TNomAlbum(Item2).Annea;
end;
```

### Récupération des artistes

```pascal
procedure TForm1.LoadArtistsFromDirs;
var
  i: integer;
  Artistes: TStringList;
begin
  ListBoxArtistes.Items.Clear;
  
  // Récupère tous les répertoires
  Artistes := FindAllDirectories(CheminMorceaux, false);
  Artistes.Sort;
  
  for i := 0 to Artistes.Count - 1 do
    ListBoxArtistes.Items.Add(ExtractFileName(Artistes[i]));
  
  Artistes.Free;
end;
```

### Récupération des albums avec tri

```pascal
procedure TForm1.LoadAlbumsFromDirs(ArtisteName: string);
var
  i, j, An, CptAlbum: integer;
  SDir: string;
  NomAlbums: TStringList;
  NameAlb: TList;
  TLS: TStringList;
  AlbumObj: TNomAlbum;
begin
  SDir := CheminMorceaux + ArtisteName;
  
  // Récupère tous les répertoires d'albums
  NomAlbums := FindAllDirectories(SDir, false);
  NameAlb := TList.Create;
  TLS := TStringList.Create;
  
  try
    for j := 0 to NomAlbums.Count - 1 do
    begin
      // Charge l'année depuis TagsParam.txt
      TLS.LoadFromFile(NomAlbums[j] + '\TagsParam.txt');
      An := StrToInt(TLS[0]);
      
      // Crée un objet d'album
      AlbumObj := TNomAlbum.Create(NomAlbums[j], An);
      NameAlb.Add(AlbumObj);
    end;
    
    // Trie les albums par année
    NameAlb.Sort(@CompareAlbums);
    
    // Remplir le tableau Albums
    SetLength(Albums, TotalAlbum + NameAlb.Count);
    
    for i := 0 to NameAlb.Count - 1 do
    begin
      with Albums[TotalAlbum + i] do
      begin
        NomArtiste := ArtisteName;
        NomAlbum := ExtractFileName(
          TNomAlbum(NameAlb[i]).Name);
        Annea := TNomAlbum(NameAlb[i]).Annea;
        NbAlbumArtiste := NameAlb.Count;
        NoOrdreAlbCetArtiste := i + 1;
      end;
    end;
    
    TotalAlbum := TotalAlbum + NameAlb.Count;
    
  finally
    TLS.Free;
    NomAlbums.Free;
    
    // Libère les objets de la liste
    for i := 0 to NameAlb.Count - 1 do
      TNomAlbum(NameAlb[i]).Free;
    NameAlb.Free;
  end;
end;
```

---

## Prochaines étapes

Voir [05-interface-pagination.md](05-interface-pagination.md) pour l'implémentation de la pagination.
