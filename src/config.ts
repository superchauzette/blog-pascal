export const SITE = {
  website: "https://superchauzette.github.io/blog-pascal/", // replace this with your deployed domain
  author: "Tillot Alain",
  profile: "https://satnaing.dev/",
  desc: "Un blog dédié à l'apprentissage du langage Pascal.",
  title: "Pacal Tuto Blog",
  ogImage: "astropaper-og.jpg",
  lightAndDarkMode: false,
  postPerIndex: 8,
  postPerPage: 8,
  scheduledPostMargin: 15 * 60 * 1000, // 15 minutes
  showSearch: false,
  showArchives: false,
  showBackButton: true, // show back button in post detail
  editPost: {
    enabled: true,
    text: "Edit page",
    url: "https://github.com/superchauzette/blog-pascal/edit/main/",
  },
  dynamicOgImage: true,
  dir: "auto", // "rtl" | "auto"
  lang: "fr", // html lang code. Set this empty and default will be "en"
  timezone: "Europe/Paris", // Default global timezone (IANA format) https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
} as const;
