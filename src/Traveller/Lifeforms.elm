module Traveller.Lifeforms exposing (biocomplexityDescription, biodiversityDescription)


biodiversityDescription : Int -> String
biodiversityDescription code =
    case code of
        0 ->
            "Sterile – No detectable biomass. Lifeless rock, ice, or gas; may have organics but no living organisms."

        1 ->
            "Trace Life – Microbes or simple extremophiles in niche environments; no visible macroscopic life."

        2 ->
            "Minimal – Simple ecosystems with only a few hardy species; mostly barren terrain with scattered life pockets."

        3 ->
            "Sparse – Low diversity; a small number of dominant species across most environments."

        4 ->
            "Limited – Multiple species present, but ecosystems remain simple and repetitive."

        5 ->
            "Moderate – Distinct ecosystems present, but limited species turnover; a ‘familiar’ set of organisms across large areas."

        6 ->
            "Developing – Several well-defined ecosystems with moderate inter-species complexity; seasonal or regional variety."

        7 ->
            "Rich – Diverse biomes with strong species variety; food webs are complex but not maximal."

        8 ->
            "Very Rich – Highly varied ecosystems with strong regional specialisation; many niches filled."

        9 ->
            "Abundant – Dense, highly interconnected food webs; ecosystems recover quickly from disruption."

        10 ->
            "Terra-Equivalent – Biodiversity equal to pre-human Earth; extremely complex, stable, and resilient biosphere."

        _ ->
            "Hyperdiverse – More complex than pre-human Earth; alien biochemistries or evolutionary histories have produced extreme species variety."


biocomplexityDescription : Int -> String
biocomplexityDescription rating =
    case rating of
        1 ->
            "Primitive single-cell organisms – Procaryotes such as bacteria and archaea."

        2 ->
            "Advanced cellular organisms – Eucaryotes such as algae and amoebae."

        3 ->
            "Primitive multicellular organisms – Lichen, sponges, some fungi."

        4 ->
            "Differentiated multicellular organisms – Ediacaran and early Cambrian age creatures."

        5 ->
            "Complex multicellular organisms – Insects, ferns, fish."

        6 ->
            "Advanced multicellular organisms – Reptiles, birds, mammals, flowering plants."

        7 ->
            "Socially advanced organisms – Ants, bees, primates, elephants."

        8 ->
            "Mentally advanced organisms – Sophont intelligence, psionics."

        9 ->
            "Extant or extinct sophonts – At some point, this world had sophont lifeforms."

        10 ->
            "Ecosystem-wide superorganisms – Cooperative sophont-like behaviour."

        _ ->
            "Unknown biocomplexity rating."
