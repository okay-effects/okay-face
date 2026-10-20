# generates core/src/main/scala/okay/face/RecordWords.scala: `record(f1, ..., fn)(make)(unmake)` for arities 1 to 12
# run from the repository's root: python3 project/records.py
out=['package okay.face','','/** `record(f1, ..., fn)(make)(unmake)`: a `Shape` of a record of n fields — GENERATED (project/records.py), arities 1 to 12 */','trait RecordWords {']
for n in range(1,13):
    As=[f'A{i}' for i in range(1,n+1)]
    tps=', '.join(As+['P'])
    params=', '.join(f'f{i}: Shape.Field[A{i}]' for i in range(1,n+1))
    if n==1:
        make='make: A1 => P'; unmake='unmake: P => A1'
        pure='Shape.Fields.Pure[A1 => P](make)'
        write='Vector(f1.name -> f1.shape.encode(unmake(p)))'
    else:
        make=f'make: ({", ".join(As)}) => P'; unmake=f'unmake: P => ({", ".join(As)})'
        curried=' => '.join(As+['P'])
        lam=' => '.join(f'(a{i}: A{i})' for i in range(1,n+1))+' => make('+', '.join(f'a{i}' for i in range(1,n+1))+')'
        pure=f'Shape.Fields.Pure[{curried}]({lam})'
        write='{ val t = unmake(p); Vector('+', '.join(f'f{i}.name -> f{i}.shape.encode(t._{i})' for i in range(1,n+1))+') }'
    fields=pure
    for i in range(1,n+1): fields=f'Shape.Fields.Ap({fields}, f{i})'
    out.append(f'  def record[{tps}]({params})({make})({unmake}): Shape[P] =')
    out.append(f'    Shape.Record[P]({fields}, (p: P) => {write})')
out.append('}')
open('core/src/main/scala/okay/face/RecordWords.scala','w').write('\n'.join(out)+'\n')
