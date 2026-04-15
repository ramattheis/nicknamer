#' Download and cache the nickname dictionary
#'
#' @description
#' Downloads the nickname list maintained by Carlton Northern
#' (\url{https://github.com/carltonnorthern/nicknames}) and returns it as a
#' tidy data frame mapping each nickname to its canonical form.  The dictionary
#' is cached for the duration of the R session so subsequent calls are
#' instantaneous.
#'
#' The raw data format is `canonical, "has_nickname", nickname`.  This
#' function inverts the relationship so each row represents a
#' \emph{nickname → canonical} mapping.  A `sex` column is appended using a
#' hardcoded lookup of common English given names; entries whose canonical
#' form is not in the lookup receive `NA` for sex.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{nickname}{Lowercase nickname string.}
#'     \item{canonical}{Lowercase canonical (full) first name.}
#'     \item{sex}{`"male"`, `"female"`, or `NA`.}
#'   }
#' @export
load_nickname_dictionary <- function() {

  # Return cached version if available
  if (!is.null(.nicknamerenv$nickname_dictionary_cache)) {
    return(.nicknamerenv$nickname_dictionary_cache)
  }

  url <- paste0(
    "https://raw.githubusercontent.com/carltonnorthern/",
    "nicknames/master/names.csv"
  )

  message("Downloading nickname dictionary...")

  tmp <- tempfile(fileext = ".csv")

  tryCatch({
    download.file(url, destfile = tmp, quiet = TRUE)

    raw <- read.csv(tmp, header = FALSE, stringsAsFactors = FALSE,
                    col.names = c("canonical", "relationship", "nickname"))

    # Keep only "has_nickname" rows and relevant columns
    raw <- raw[raw$relationship == "has_nickname", c("canonical", "nickname"),
               drop = FALSE]

    # Lowercase and trim
    raw$canonical <- tolower(trimws(raw$canonical))
    raw$nickname  <- tolower(trimws(raw$nickname))

    # Remove any empty strings
    raw <- raw[nchar(raw$canonical) > 0 & nchar(raw$nickname) > 0, ]

    # Hardcoded sex lookup for common English given names
    male_names <- c(
      "aaron", "abraham", "adam", "albert", "alexander", "alfred", "andrew",
      "archibald", "augustus", "barnabas", "bartholomew", "benjamin", "caleb",
      "carl", "charles", "christian", "christopher", "clement", "cornelius",
      "cyrus", "daniel", "david", "dennis", "donald", "douglas", "edgar",
      "edward", "elijah", "elisha", "ernest", "eugene", "ezekiel", "ezra",
      "felix", "ferdinand", "francis", "frank", "franklin", "frederick",
      "george", "gerald", "gordon", "harold", "harry", "henry", "herbert",
      "herman", "hiram", "homer", "horace", "howard", "humphrey", "isaac",
      "isaiah", "jacob", "james", "jasper", "jeremiah", "jerome", "joel",
      "john", "jonathan", "joseph", "josiah", "julius", "kenneth", "lambert",
      "lawrence", "leonard", "lewis", "louis", "marcus", "martin", "matthew",
      "michael", "miles", "morgan", "moses", "nathaniel", "nicholas", "noah",
      "norbert", "norman", "oliver", "oscar", "patrick", "paul", "peter",
      "philip", "ralph", "raymond", "reginald", "richard", "robert", "roger",
      "rudolph", "samuel", "sebastian", "simon", "solomon", "stephen",
      "sylvester", "theodore", "thomas", "timothy", "tobias", "victor",
      "vincent", "walter", "warren", "wendell", "william", "zachary"
    )

    female_names <- c(
      "abigail", "adelaide", "agatha", "agnes", "alice", "alexandra",
      "amanda", "amelia", "anastasia", "ann", "anna", "anne", "arabella",
      "barbara", "beatrice", "bertha", "caroline", "carolyn", "catharine",
      "catherine", "cecilia", "charlotte", "clara", "clemence", "constance",
      "cordelia", "dorothy", "edna", "eleanor", "eliza", "elizabeth", "ella",
      "ellen", "eloise", "emily", "emma", "ernestine", "esther", "ethel",
      "eugenia", "eva", "evelyn", "fannie", "fanny", "felicia", "flora",
      "florence", "frances", "gertrude", "grace", "harriet", "helen",
      "henrietta", "hester", "ida", "irene", "isabel", "isabella", "jane",
      "janet", "jean", "jennie", "jenny", "jessie", "joan", "josephine",
      "judith", "julia", "juliet", "kate", "katharine", "katherine", "laura",
      "lavinia", "leonora", "louisa", "louise", "lucia", "lucy", "lydia",
      "mabel", "margaret", "maria", "marian", "marianne", "martha", "mary",
      "matilda", "maud", "maude", "millicent", "minnie", "miriam", "nancy",
      "nellie", "olive", "patricia", "pauline", "pearl", "penelope", "phyllis",
      "priscilla", "rachel", "rebecca", "rhoda", "rosa", "rosalie", "rosanna",
      "rose", "ruth", "sallie", "samantha", "sarah", "selina", "sophia",
      "sue", "susan", "susanna", "susannah", "sylvia", "theresa", "virginia",
      "wilhelmina", "winifred"
    )

    sex_lookup <- c(
      setNames(rep("male",   length(male_names)),   male_names),
      setNames(rep("female", length(female_names)), female_names)
    )

    raw$sex <- sex_lookup[raw$canonical]

    # The carltonnorthern dataset is bidirectional — it contains both
    # bill→william and william→bill.  We only keep mappings whose canonical
    # is a recognised full given name (in our sex lookup), so that informal
    # forms always point *to* formal ones, never the other way around.
    # e.g. bill→william ✓, william→bill ✗; bob→robert ✓, robert→bob ✗.
    known_full_names <- c(male_names, female_names)
    raw <- raw[raw$canonical %in% known_full_names, ]

    # When a nickname maps to multiple canonicals of the same sex, prefer the
    # one with the most nickname entries in the dataset (most "prominent" full
    # name).  This resolves e.g. bill→william over bill→robert.
    prominence <- table(raw$canonical)
    raw$prominence <- as.integer(prominence[raw$canonical])
    raw <- raw[order(-raw$prominence), ]
    raw <- raw[!duplicated(raw[, c("nickname", "sex")]), ]
    raw$prominence <- NULL
    rownames(raw) <- NULL

    # Manual corrections for cases where the bidirectional dataset produces
    # the wrong mapping (e.g. bill → robert should be bill → william).
    corrections <- list(bill = "william", billy = "william", billa = "william")
    for (nick in names(corrections)) {
      canon <- corrections[[nick]]
      if (canon %in% known_full_names) {
        raw$canonical[raw$nickname == nick] <- canon
        raw$sex[raw$nickname == nick]       <- sex_lookup[canon]
      }
    }

    # Cache and return
    .nicknamerenv$nickname_dictionary_cache <- raw
    return(raw)

  }, error = function(e) {
    if (file.exists(tmp)) file.remove(tmp)
    stop("Failed to download nickname dictionary: ", e$message)
  }, finally = {
    if (file.exists(tmp)) file.remove(tmp)
  })
}
