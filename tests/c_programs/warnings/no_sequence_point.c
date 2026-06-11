/* Sequenced or single modifications must NOT trigger -Wsequence-point. Runs cleanly to 0. */
int main(void) {
    int i = 0;
    i = i + 1;               /* single modification + a read */
    int a = i++;             /* single modification */
    int b = i ? i++ : i--;   /* ?: is a sequence point */
    int c = (i++) && (i++);  /* && is a sequence point */
    i++;
    i++;                     /* separate statements = separate full expressions */
    return 0;
}
