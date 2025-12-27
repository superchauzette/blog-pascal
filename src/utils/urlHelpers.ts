/**
 * Utilitaire pour gérer les URLs avec la base configurée dans Astro
 */

// Utilise la variable d'environnement fournie par Astro
const BASE_PATH = import.meta.env.BASE_URL || "/";

/**
 * Retourne le chemin avec la base path préfixée
 * @param path - Le chemin à préfixer
 * @returns Le chemin avec la base URL configurée
 */
export function withBase(path: string): string {
  // Si le chemin contient déjà la base, retourne tel quel
  if (BASE_PATH !== "/" && path.startsWith(BASE_PATH)) {
    return path;
  }

  // Pour la racine
  if (path === "/") {
    return BASE_PATH === "/" ? "/" : BASE_PATH + "/";
  }

  // Pour les autres chemins
  const cleanPath = path.startsWith("/") ? path : "/" + path;
  return BASE_PATH === "/" ? cleanPath : BASE_PATH + cleanPath;
}

/**
 * Retourne la base path
 */
export function getBasePath(): string {
  return BASE_PATH;
}

/**
 * Vérifie si une URL est active
 */
export function isActive(currentPath: string, targetPath: string): boolean {
  const cleanCurrent =
    currentPath.endsWith("/") && currentPath !== "/"
      ? currentPath.slice(0, -1)
      : currentPath;
  const cleanTarget =
    targetPath.endsWith("/") && targetPath !== "/"
      ? targetPath.slice(0, -1)
      : targetPath;

  // Compte la base path si elle est présente
  const currentWithoutBase = cleanCurrent.replace(BASE_PATH, "");
  const targetWithoutBase = cleanTarget.replace(BASE_PATH, "");

  return (
    currentWithoutBase === targetWithoutBase ||
    currentWithoutBase.split("/")[1] === targetWithoutBase.split("/")[1]
  );
}
