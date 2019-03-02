.code

callFunc proc 
  push rbp
  mov rbp, rsp
  mov r10, rcx
  mov r11, rdx
  mov rax, r8
  cmp rax,4
  jng ARG4
  test rax,1
  jz ARGOVER4
  sub rsp, 8
ARGOVER4:
  cmp rax, 4
  jng ARG4
  mov rcx, [r11]
  push rcx
  sub r11, 8
  dec rax
  jmp ARGOVER4
ARG4:
  cmp rax,3
  jng PUSHR9
  mov r9, [r11]
  sub r11, 8
  dec rax
PUSHR9:
  push r9
  cmp rax,2
  jng PUSHR8
  mov r8, [r11]
  sub r11, 8
  dec rax
PUSHR8:
  push r8
  cmp rax, 1
  jng PUSHRDX
  mov rdx, [r11]
  sub r11, 8
  dec rax
PUSHRDX:
  push rdx
  cmp rax, 0
  jng PUSHRCX
  mov rcx, [r11]
PUSHRCX:
  push rcx
  mov rax,0
  call r10
  mov rsp, rbp
  pop rbp
  ret
callFunc endp

end