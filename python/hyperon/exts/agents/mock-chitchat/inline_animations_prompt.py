

ia_prompt="""Generate a dialog that includes natural language responses along with emotional facial expressions and animations. The output should combine conversational text with appropriately timed and expressive commands to simulate lifelike interaction.

Facial Expressions:
Add commands to reflect the speaker's emotions using the format: |[FACIAL_EXPRESSION], [DURATION_SEC], [MAGNITUDE]| Select a duration within a natural conversational range (e.g., {DURATION_RANGE} seconds). Select a magnitude within the range {NAGNITUDE_RANGE}, based on the intensity of the emotion

Gestures:
Include gestures (animations) where appropriate to emphasize words, present ideas, or respond to the dialog context. Use the animation format: |[ANIMATION]| Insert gestures naturally in the flow of conversation.

Facial expressions and gestures should enhance the meaning or tone of the spoken words.

You are restricted to using only the facial expressions provided in the following list: {FACIAL_EXPRESSIONS}. No other expressions should be used under any circumstances.

You are restricted to using only the gestures (animations) provided in the following list: {LIST_ANIMATIONS}. No other animations should be used under any circumstances. Use animations only when appropriate, and avoid repeatedly using the same one. Try to use different animations! For example do not use both_cut animation after each phrase, try to use different animations!

There is a short description for each animation:
{ANIMATIONS_WITH_DESCRIPTIONS}

Several examples of generated responses with facial expressions and animations:
Hello |smile, 1.5, 0.6|! I’m Alex. It’s a pleasure to meet you |both_cut|.
Absolutely |yes_eager|, I couldn’t agree more! This is such a brilliant idea |left_idea|.
Hmm |confused, 1, 0.5|, I see what you mean. That’s a bit tricky |both_double_cut|.
Yes, we did it |yes_once|! This is amazing |left_thumbs_up|!
No |no_subtle|, I don’t think that’s quite right |not_at_all|."""

def generate_ia_prompt(config):
    simple_animation_names = [s.split(":", 1)[0].strip().lower() for s in config.get("speech_generator.inline_animation.simple_animations")]


    data = {"NAGNITUDE_RANGE" : config.get("speech_generator.inline_animation.proposed_magnitude_range"),
            "DURATION_RANGE"  : config.get("speech_generator.inline_animation.proposed_duration_range"),
            "FACIAL_EXPRESSIONS": ", ".join(config.get("speech_generator.inline_animation.facial_expressions")),
            "LIST_ANIMATIONS": ", ".join(simple_animation_names),
            "ANIMATIONS_WITH_DESCRIPTIONS": "\n".join(config.get("speech_generator.inline_animation.simple_animations"))}
    return ia_prompt.format(**data)
