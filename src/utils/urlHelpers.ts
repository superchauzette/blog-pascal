/**
 * Utilitaire pour gérer les URLs avec la base /blog-pascal
 */

const BASE_PATH = "/blog-pascal";

/**
 * Retourne le chemin avec la base path préfixée
 * @param path - Le chemin à préfixer
 * @returns Le chemin avec /blog-pascal
 */
export function withBase(path: string): string {
  if (path.startsWith(BASE_PATH)) {
    return path;
  }
  if (path === "/") {
    return BASE_PATH + "/";
  }
  return BASE_PATH + path;
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
