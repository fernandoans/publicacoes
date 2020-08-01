function [vetor] = ordenaVetor(vetor)
  troca = false
  while true
    for i = 1:length(vetor) - 1
      if vetor(i+1) < vetor(i)
        x = vetor(i+1)
        vetor(i+1) = vetor(i)
        vetor(i) = x
        troca = true
      endif
    endfor
    if !troca
      break
    else
      troca = false
    endif
  endwhile
endfunction
