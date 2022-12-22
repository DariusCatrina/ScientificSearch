package ai.lum.odinson.extra
import ai.lum.odinson.{EventMatch, ExtractorEngine, NamedCapture, OdinsonMatch}
import ai.lum.odinson.utils.DisplayUtils.displayMention


object Main2 extends App {
// Initialize the extractor engine -- ensure that your config still has `odinson.indexDir` pointing
// to where you wrote your index, here we were using data/pets/index
val extractorEngine = ExtractorEngine.fromConfig()

// Here we have a set of two rules, which will first find `Pet` mentions, and the find 
// `Adoption` Mentions.
val rules = """
    |rules:
    |  - name: pets_type
    |    type: basic
    |    label: Pet  # if found, will have the label "Pet"
    |    priority: 1 # will run in the first round of extraction
    |    pattern: |
    |       [lemma=/cat|dog|bunny|fish/]
    |
    |  - name: pets_adoption
    |    type: event
    |    label: Adoption
    |    priority: 2  # will run in the second round of extraction, can reference priority 1 rules
    |    pattern: |
    |      trigger = [lemma=adopted]
    |      adopter = >nsubj []   # note: we didn't specify the label, so any token will work
    |      pet: Pet = >dobj []
    """.stripMargin

// Compile the rules into Extractors that will be used with the Index
val extractors = extractorEngine.compileRuleString(rules)

// Extract Mentions
val mentions = extractorEngine.extractMentions(extractors)

// Display the mentions
mentions.foreach(displayMention(_, extractorEngine))
}