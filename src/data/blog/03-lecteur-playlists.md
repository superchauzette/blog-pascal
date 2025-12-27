---
title: Lecteur Audio et Gestion des Playlists en Pascal
author: Tillot Alain
pubDatetime: 2025-12-20T15:33:05.569Z
slug: how-to-code-pascal-03-lecteur-playlists
featured: false
draft: false
ogImage: ../../assets/images/forrest-gump-quote.png
tags:
  - Pascal
  - Lazarus
  - Audio
  - Playlists
description: Implémentation du lecteur Windows Media Player et gestion complète des playlists.
---

# Lecteur Audio et Gestion des Playlists

## Table des matières

1. [Lecteur Windows Media Player](#lecteur-windows-media-player)
2. [Gestion des titres](#gestion-des-titres)
3. [Gestion des playlists](#gestion-des-playlists)
4. [Interface de playlist](#interface-de-playlist)

---

## Lecteur Windows Media Player

### Initialisation du lecteur

```pascal
var
  WMP: OleVariant;
  ServerName: string = 'WMPlayer.OCX';

procedure TForm1.InitializeMediaPlayer;
begin
  if Assigned(InitProc) then
    TProcedure(InitProc);
  
  try
    WMP := CreateOleObject(ServerName);
  except
    ShowMessage('Impossible de démarrer WMP');
    Exit;
  end;
end;
```

### Commandes de base

```pascal
// Définir le fichier à jouer
WMP.URL := 'C:\Musique\Artiste\Album\Chanson.mp3';

// Lecture
WMP.Controls.Play;

// Arrêt
WMP.Controls.Stop;

// Pause
WMP.Controls.Pause;

// État de lecture
if VarToStr(WMP.PlayState) = '1' then
  ShowMessage('Le titre est terminé');
```

### États de lecture

```pascal
const
  wmppsStopped = 0;      // Arrêté
  wmppsPlaying = 1;      // En cours de lecture
  wmppsPaused = 2;       // En pause
  wmppsWaiting = 3;      // En attente
  wmppsScanForward = 4;  // Avance rapide
  wmppsScanReverse = 5;  // Retour rapide
  wmppsClosed = 6;       // Fermé
```

---

## Gestion des titres

### Structure de données pour un titre

```pascal
type
  TitreInfo = record
    Artiste: string;
    Album: string;
    Titre: string;
    NumPiste: integer;
    Chemin: string;
  end;
```

### Chargement des titres d'un album

```pascal
procedure TForm1.ListBoxTitresClick(Sender: TObject);
var
  IndexTitre: integer;
  TitreInfo: TitreInfo;
  CheminFichier: string;
begin
  IndexTitre := ListBoxTitres.ItemIndex;
  
  if IndexTitre >= 0 then
  begin
    // Construit le chemin du fichier
    CheminFichier := CheminMorceaux + ArtisteSelect + '\' + 
                     AlbumSelect + '\' + 
                     ListBoxTitres.Items[IndexTitre] + '.mp3';
    
    // Lance la lecture
    WMP.URL := CheminFichier;
    WMP.Controls.Play;
    
    // Met à jour l'affichage
    UpdateNowPlayingDisplay(ArtisteSelect, AlbumSelect, 
                           ListBoxTitres.Items[IndexTitre]);
  end;
end;

// Double-clic pour jouer l'album entier
procedure TForm1.ListBoxTitresDblClick(Sender: TObject);
begin
  PlayEntireAlbum(ArtisteSelect, AlbumSelect);
end;
```

### Lecture d'un album complet avec Timer

```pascal
type
  PlayListEntry = record
    Artiste: string;
    Album: string;
    Titre: string;
  end;

var
  PL: array of PlayListEntry;  // Playlist en mémoire
  CurrentTrackIndex: integer = 0;
  TimerAutoNext: TTimer;

procedure TForm1.PlayEntireAlbum(Artiste, Album: string);
var
  i: integer;
  NameFich: string;
  TL: TStringList;
  NbTitres: integer;
begin
  // Réinitialise la playlist
  SetLength(PL, 0);
  
  // Charge les titres de l'album
  NameFich := CheminMorceaux + Artiste + '\' + Album + '\TagsParam.txt';
  TL := TStringList.Create;
  try
    if FileExists(NameFich) then
    begin
      TL.LoadFromFile(NameFich);
      TL.Delete(0);  // Année
      NbTitres := StrToInt(TL[0]);
      TL.Delete(0);
      
      // Remplir la playlist
      SetLength(PL, NbTitres);
      for i := 0 to NbTitres - 1 do
      begin
        PL[i].Artiste := Artiste;
        PL[i].Album := Album;
        PL[i].Titre := TL[i];
      end;
    end;
  finally
    TL.Free;
  end;
  
  // Lance la lecture du premier titre
  CurrentTrackIndex := 0;
  PlayTrackAtIndex(0);
  
  // Active le timer pour gérer la transition automatique
  TimerAutoNext.Enabled := true;
end;

procedure TForm1.PlayTrackAtIndex(Index: integer);
var
  CheminFichier: string;
begin
  if (Index >= 0) and (Index < Length(PL)) then
  begin
    CurrentTrackIndex := Index;
    
    CheminFichier := CheminMorceaux + PL[Index].Artiste + '\' + 
                     PL[Index].Album + '\' + PL[Index].Titre + '.mp3';
    
    WMP.URL := CheminFichier;
    WMP.Controls.Play;
    
    UpdateNowPlayingDisplay(PL[Index].Artiste, PL[Index].Album, 
                           PL[Index].Titre);
  end;
end;

// Timer pour vérifier la fin du titre
procedure TForm1.TimerAutoNextTimer(Sender: TObject);
begin
  // Vérifie si le titre est terminé
  if VarToStr(WMP.PlayState) = '1' then  // État terminé
  begin
    // Passe au titre suivant
    if CurrentTrackIndex < Length(PL) - 1 then
      PlayTrackAtIndex(CurrentTrackIndex + 1)
    else
      TimerAutoNext.Enabled := false;  // Fin de la playlist
  end;
end;
```

---

## Gestion des playlists

### Structure d'une playlist

```pascal
type
  PlayListEntry = record
    Artiste: string;
    Album: string;
    Titre: string;
  end;

var
  CurrentPlayList: array of PlayListEntry;
```

### Interface de la playlist

Le panel GrillePL contient :

- **FListBox** pour les playlists sauvegardées
- **ListBox** pour les titres de la playlist active
- Boutons de gestion (supprimer, vider, sauvegarder)
- Champ de saisie du nom

### Configuration des composants

```pascal
procedure TForm1.SetupPlaylistUI;
begin
  // Configure la ListBox pour le drag-and-drop
  ListBoxTitres.DragMode := dmAutomatic;
  GrillePL.DragMode := dmAutomatic;
  
  // Configure le style pour utiliser OnDrawItem
  GrillePL.Style := lbOwnerDrawFixed;
end;
```

---

## Opérations sur les playlists

### Ajouter des titres par Drag & Drop

```pascal
procedure TForm1.GrillePLDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Index: integer;
  SDir: string;
begin
  if Source = ListBoxTitres then
  begin
    Index := ListBoxTitres.ItemIndex;
    
    if Index >= 0 then
    begin
      // Ajoute le titre à la playlist
      SetLength(CurrentPlayList, Length(CurrentPlayList) + 1);
      with CurrentPlayList[High(CurrentPlayList)] do
      begin
        Artiste := ArtisteSelect;
        Album := AlbumSelect;
        Titre := ListBoxTitres.Items[Index];
      end;
      
      // Met à jour l'affichage
      UpdatePlaylistDisplay;
    end;
  end;
end;

procedure TForm1.GrillePLDragOver(Sender, Source: TObject; X, Y: Integer;
                                  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = ListBoxTitres);
end;
```

### Affichage personnalisé de la grille

```pascal
procedure TForm1.GrillePLDrawItem(Control: TWinControl; Index: Integer;
                                  ARect: TRect; State: TOwnerDrawState);
begin
  with GrillePL.Canvas do
  begin
    FillRect(ARect);
    
    if GrillePL.Selected[Index] then
      Font.Color := clWhite
    else
      Font.Color := clBlack;
    
    // Affiche 3 colonnes : Artiste | Album | Titre
    TextOut(ARect.Left, ARect.Top, 
            Copy(CurrentPlayList[Index].Artiste, 1, 26));
    TextOut(ARect.Left + 250, ARect.Top, 
            Copy(CurrentPlayList[Index].Album, 1, 30));
    TextOut(ARect.Left + 560, ARect.Top, 
            Copy(CurrentPlayList[Index].Titre, 1, 30));
  end;
end;
```

---

## Gestion des fichiers playlist

### Format WPL (Windows Playlist)

```pascal
procedure TForm1.SavePlaylistAsWPL(PlaylistName: string);
var
  SL: TStringList;
  i: integer;
  CheminFichier: string;
begin
  SL := TStringList.Create;
  try
    // En-tête WPL
    SL.Add('<?xml version="1.0" encoding="UTF-8"?>');
    SL.Add('<smil>');
    SL.Add('  <head>');
    SL.Add('    <title>' + PlaylistName + '</title>');
    SL.Add('  </head>');
    SL.Add('  <body>');
    SL.Add('    <seq>');
    
    // Ajoute chaque titre
    for i := 0 to High(CurrentPlayList) do
    begin
      with CurrentPlayList[i] do
      begin
        SL.Add('      <media src="' + CheminMorceaux + Artiste + 
               '\' + Album + '\' + Titre + '.mp3" />');
      end;
    end;
    
    SL.Add('    </seq>');
    SL.Add('  </body>');
    SL.Add('</smil>');
    
    // Sauvegarde le fichier
    CheminFichier := CheminPlayListe + PlaylistName + '.wpl';
    SL.SaveToFile(CheminFichier);
  finally
    SL.Free;
  end;
end;
```

### Charger une playlist existante

```pascal
procedure TForm1.LoadPlaylistFromFile(PlaylistName: string);
var
  NamePlay: string;
  TL: TStringList;
  i, LineCount: integer;
  SrcPath: string;
  Artist, Album, Titre: string;
begin
  TL := TStringList.Create;
  try
    NamePlay := CheminPlayListe + PlaylistName + '.wpl';
    
    if FileExists(NamePlay) then
    begin
      TL.LoadFromFile(NamePlay);
      SetLength(CurrentPlayList, 0);
      
      // Parse le fichier WPL
      for i := 0 to TL.Count - 1 do
      begin
        if Pos('<media src=', TL[i]) > 0 then
        begin
          // Extrait le chemin
          SrcPath := ExtractBetween(TL[i], '"', '"');
          
          // Parse le chemin
          ExtractArtistAlbumTitre(SrcPath, Artist, Album, Titre);
          
          // Ajoute à la playlist
          SetLength(CurrentPlayList, Length(CurrentPlayList) + 1);
          with CurrentPlayList[High(CurrentPlayList)] do
          begin
            Artiste := Artist;
            Album := Album;
            Titre := Titre;
          end;
        end;
      end;
      
      UpdatePlaylistDisplay;
    end;
  finally
    TL.Free;
  end;
end;
```

### Procédures de gestion

```pascal
// Supprimer une ligne de la playlist
procedure TForm1.BtDelLineClick(Sender: TObject);
var
  Index: integer;
  i: integer;
begin
  Index := GrillePL.ItemIndex;
  
  if Index >= 0 then
  begin
    // Décale les éléments
    for i := Index to High(CurrentPlayList) - 1 do
      CurrentPlayList[i] := CurrentPlayList[i + 1];
    
    SetLength(CurrentPlayList, Length(CurrentPlayList) - 1);
    UpdatePlaylistDisplay;
  end;
end;

// Vider la playlist
procedure TForm1.BtEmptyPlaylistClick(Sender: TObject);
begin
  SetLength(CurrentPlayList, 0);
  UpdatePlaylistDisplay;
end;

// Sauvegarder la playlist
procedure TForm1.BtSavePlaylistClick(Sender: TObject);
var
  PlaylistName: string;
begin
  PlaylistName := EditPlaylistName.Text;
  
  if PlaylistName <> '' then
  begin
    SavePlaylistAsWPL(PlaylistName);
    ShowMessage('Playlist sauvegardée : ' + PlaylistName);
    LoadPlaylistList;  // Rafraîchit la liste
  end
  else
    ShowMessage('Entrez un nom pour la playlist');
end;

// Supprimer une playlist
procedure TForm1.BtDeletePlaylistClick(Sender: TObject);
var
  Index: integer;
  CheminFichier: string;
begin
  Index := FileListBoxPL.ItemIndex;
  
  if Index >= 0 then
  begin
    CheminFichier := CheminPlayListe + FileListBoxPL.Items[Index] + '.wpl';
    
    if FileExists(CheminFichier) then
    begin
      DeleteFile(CheminFichier);
      LoadPlaylistList;
      ShowMessage('Playlist supprimée');
    end;
  end;
end;

// Jouer la playlist
procedure TForm1.BtPlayPlaylistClick(Sender: TObject);
begin
  if Length(CurrentPlayList) > 0 then
  begin
    PlayListTrack(0);
    TimerAutoNext.Enabled := true;
  end;
end;

procedure TForm1.PlayListTrack(Index: integer);
begin
  if (Index >= 0) and (Index < Length(CurrentPlayList)) then
  begin
    CurrentTrackIndex := Index;
    PlayTrackAtIndex(Index);
  end;
end;
```

### Mise à jour de l'affichage

```pascal
procedure TForm1.UpdatePlaylistDisplay;
begin
  GrillePL.Items.Clear;
  GrillePL.Refresh;
end;

procedure TForm1.UpdateNowPlayingDisplay(Artiste, Album, Titre: string);
begin
  LblNowPlayingArtist.Caption := Artiste;
  LblNowPlayingAlbum.Caption := Album;
  LblNowPlayingTitle.Caption := Titre;
  CoverNowPlaying.Invalidate;
end;
```

---

## Prochaines étapes

Voir [04-sources-donnees.md](04-sources-donnees.md) pour l'implémentation des différentes sources de données.
