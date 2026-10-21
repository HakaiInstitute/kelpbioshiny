# kelpbioshiny writing style

How the text the app shows is written: titles, labels, buttons, notices, help popovers, the welcome card and the User guide, and the README. kelpbio's `STYLE.md` (`~/Code/HakaiInstitute/kelpbio/STYLE.md`) applies: its principles, vocabulary, and mechanics. This file lists what differs for an app and the app's own terms. The statistics are kelpbio's, so the app links to its articles rather than restating them.

## Principles

1. Write for a field biologist using the app for the first time: what to do, what a result means, what to do about a warning. Nothing else.
2. Show text only where the page needs it: no subtitle that restates its title or step, and nothing a control's label already says.
3. Address the user with the imperative ("Choose the species", "Fit the model"). "Your data" is the name of a source; elsewhere, kelpbio's no-"you" rule applies.

## Text by element

| Element | Rule |
|---|---|
| Title, label, button, tab, badge | Sentence case, no full stop, a few words. A button starts with a verb ("Continue to models", "Fit all", "View estimates"). |
| Subtitle, notice, empty state | One short sentence with a full stop. A notice says what happened and, if anything, what to do. |
| Help popover | One to three plain-language sentences, then a "Learn more" link to the kelpbio article. |
| User guide | Short points per step, built from the app's definitions; no screenshots. |
| Warnings and errors from kelpbio | Shown as kelpbio words them: change the wording in kelpbio, not in the app. |

## Vocabulary

In addition to kelpbio's:

| Concept | Use | Not |
|---|---|---|
| The area a survey covers | area surveyed | area searched |
| A prediction without site or year effects | overall | population-level |
| Predictions for each site in each year | by site and year | by site-year (in a label), by site |
| A site of the fit's data standing in for a new site | stand-in site | representative site, borrowed site |
| A sheet, in text | its name in code font (`size_census`, `survey`) | "the Size tab" |
| A model | its label (Weight, Size, Cover calibration, Wet:dry, Carbon) | its function name |
