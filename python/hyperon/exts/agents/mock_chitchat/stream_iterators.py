def IteratorSingle(s):
    yield s

abbreviations = { 2: ['A.', 'B.', 'D.', 'J.', 'M.', 'P.', 'a.', 'e.', 'i.', 'p.'],
3: ['Co.', 'Dr.', 'Ed.', 'Eq.', 'Ex.', 'Ft.', 'Jr.', 'Lt.', 'Mr.', 'Ms.', 'Mt.', 'No.', 'Ph.', 'Pl.', 'Rd.', 'Sr.', 'St.', 'hr.', 'vs.'],
4: ['U.S.', 'A.M.', 'Adm.', 'Amb.', 'Art.', 'Ave.', 'B.A.', 'Col.', 'D.D.', 'Dir.', 'Esq.', 'Fig.', 'Gen.', 'Gov.', 'Hon.', 'Inc.', 'J.D.', 'Ltd.', 'M.A.', 'M.D.', 'Maj.', 'Mrs.', 'P.M.', 'Pvt.', 'Rep.', 'Rev.', 'Sec.', 'Sen.', 'Sgt.', 'Vol.', 'a.m.', 'e.g.', 'etc.', 'i.e.', 'min.', 'p.m.'],
5: ['Assn.', 'B.Sc.', 'Blvd.', 'Bros.', 'Capt.', 'Chap.', 'Cmdr.', 'Corp.', 'Ed.D.', 'M.Sc.', 'Ph.D.', 'Pres.', 'Prof.'],
6: ['D.D.S.'],
}


def is_certanly_sentence(s):
    if len(s) < 3:
        return False
    if s[-1] not in ['.', '!', '?']:
        return False
    if len(s) > 1 and s[-2].isdigit():
        return False
    for l in abbreviations:
        if len(s) == l and s in abbreviations[l]:
            return False
        if len(s) > l and s[-l:] in abbreviations[l] and s[-l-1] == ' ':
            return False
    if s.count("|") % 2 != 0:
        return False
    return True

def IteratorSentenceBySentence(it):
    sentence = ""
    for token in it:
        sentence += token
        sentence_strip = sentence.strip()
        if is_certanly_sentence(sentence_strip):
            yield sentence_strip
            sentence = ""
    sentence_strip = sentence.strip()
    if len(sentence_strip) > 0:
        yield sentence_strip

class ItearatorWithClose:
    def __init__(self, it, c = None):
        self.it = it
        self.c = c
    def __iter__(self):
        return self
    def __next__(self):
        return next(self.it)
    def close(self):
        if self.c is not None:
            self.c.close()
