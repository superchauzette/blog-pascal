## 🚀 Déploiement sur GitHub Pages

Le site est automatiquement déployé sur **GitHub Pages** à chaque push vers la branche `main`.

### ✅ Configuration complète

1. **GitHub Actions Workflow** : `.github/workflows/deploy.yml`
   - Construit le site Astro
   - Déploie automatiquement sur GitHub Pages

2. **Configuration Astro** : `astro.config.ts`
   - Site statique configuré
   - Optimisé pour GitHub Pages

3. **Repository Settings**
   - GitHub Pages activé via Actions
   - URL publique : `https://superchauzette.github.io/blog-pascal/`

### 📝 Modification des articles

Pour ajouter ou modifier un article :

```bash
# 1. Créer/modifier un fichier .md dans src/data/blog/
git add src/data/blog/nouveau-article.md

# 2. Commiter
git commit -m "docs: add new article"

# 3. Pousser
git push origin main
```

Le site sera automatiquement reconstruit et déployé ! ✨

### 📂 Structure des articles

Tous les articles du tutoriel Pascal se trouvent ici :
```
src/data/blog/
├── 00-index.md                          # Index principal
├── 01-introduction.md                   # Chapitre 1
├── 02-gestion-artistes-albums.md        # Chapitre 2
├── 03-lecteur-playlists.md              # Chapitre 3
├── 04-sources-donnees.md                # Chapitre 4
└── 05-interface-pagination.md           # Chapitre 5
```

### 🔗 Accès public

- **Site complet** : https://superchauzette.github.io/blog-pascal/
- **Tous les posts** : https://superchauzette.github.io/blog-pascal/posts/
- **Index tutoriel** : https://superchauzette.github.io/blog-pascal/posts/how-to-code-pascal-index

### 🛠️ Développement local

```bash
# Installer les dépendances
npm install

# Lancer le serveur de développement
npm run dev

# Construire pour la production
npm run build

# Prévisualiser la build
npm run preview
```

### 📊 Vérification du déploiement

1. Allez dans **Settings** → **Pages**
2. Vérifiez que la source est "GitHub Actions"
3. Consultez l'historique des déploiements dans **Actions** → **Deploy to GitHub Pages**

---

**Dernière mise à jour :** 2025-12-27
