import requests

for i in range(1968,2001):
    r=requests.get(f"https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_{i}.csv")
    
    if r:
        with open(f"stats/{i}.csv","w") as f:
            f.write(r.text)
    else: print(f"error: {i}")