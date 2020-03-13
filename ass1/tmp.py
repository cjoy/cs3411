def parun(arr, res):
  if len(arr) == 0:
    return []
  
  if len(res) == 0:
    res.append([arr[0]])
  elif arr[0] % 2 == 0 and res[-1][-1] % 2 == 0:
    res[-1].append(arr[0])
  elif arr[0] % 2 != 0 and res[-1][-1] % 2 != 0:
    res[-1].append(arr[0])
  elif arr[0] % 2 == 0 and res[-1][-1] % 2 != 0:
    res.append([arr[0]])
  elif arr[0] % 2 != 0 and res[-1][-1] % 2 == 0:
    res.append([arr[0]])

  return parun(arr[1:], res)

par = []
parun([], par)
print(par)
parun([8,0,4,3,7,2,-1,9,9], par)
print(par)

